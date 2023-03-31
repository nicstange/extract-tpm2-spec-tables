use std::collections::HashMap;
use std::cmp;
use std::mem;
use std::mem::swap;
use std::convert::{TryFrom, From};
use std::ops::{Add, Sub, Index, IndexMut};
use std::fmt;
use std::cell::Cell;
use std::borrow::Borrow;
use std::path;
use std::ptr;
use std::str;
use std::str::FromStr;
use regex::Regex;
use pdf::file::File;
use pdf::font::Font;
use pdf::object::Page;
use extract_tpm2_spec_tables::avl_tree::AVLTree;
use extract_tpm2_spec_tables::interval_tree::{IntervalTree, IntervalBound, Interval};
use clap::Parser;

struct LastTable {
    table: Option<Table>,
}

impl LastTable {
    fn new() -> Self {
        Self{table: None}
    }

    fn replace(&mut self, table: Option<Table>) {
        if self.table.is_some() {
            println!("ENDTABLE");
        }
        self.table = table;
    }

    fn continue_with(&mut self, table: Table) {
        assert!(self.table.is_some());
        self.table = Some(table);
    }

    fn can_continue_with(&self, next: &Table) -> bool {
        let prev = match &self.table {
            None => return false,
            Some(prev) => prev,
        };

        if prev.rows() < 1 || next.rows() < 1 || prev.columns() != next.columns() {
            return false;
        }

        for i in 0..prev.columns() {
            let prev_header = prev[(0, i)].texts.collect();
            let next_header = next[(0, i)].texts.collect();
            if !prev_header.split_whitespace().eq(next_header.split_whitespace()) {
                return false;
            }
        }
        true
    }
}

enum TableType {
    Unknown,
    TypeDef,
    CommandDef,
    ResponseDef,
    Defines,
}

#[derive(Parser, Debug)]
struct Cli {
    #[arg(name = "input-pdf-file", index(1), required = true)]
    input_pdf_file: path::PathBuf,
    #[arg(name = "source-document-name", long, short)]
    source_document_name: Option<String>,
}

fn main() {
    let cli = Cli::parse();

    let file = File::open(cli.input_pdf_file).unwrap();
    let mut i = 0;
    let mut is_first = true;
    let mut last_table = LastTable::new();
    let mut table_type = TableType::Unknown;
    let re_table_caption = Regex::new(r"(?x)
                                      ^(?:Table\s*(?P<TABLENO>[0-9]+))[^A-Z]*
                                      ((?P<TYPE>Definition\s+of.*)|
                                       (?P<COMMAND>.*Command\s*)|
                                       (?P<RESPONSE>.*Response\s*)|
                                       (?P<DEFINES>Defines\s*for.*(Values|Constants)\s*)
                                      )$").unwrap();
    let re_tag_descr = Regex::new(r"[A-Za-z0-9_]+_ST_[A-Za-z0-9_]+").unwrap();
    let re_cc_descr = Regex::new(r"[A-Za-z0-9_]+_CC_[A-Za-z0-9_]+(\s*\{(NV|F|E)(\s+(NV|F|E))*\})?").unwrap();
    let re_handle_descr = Regex::new(r"(?x)
                                      [A-Za-z0-9_]+_R[HS]_[A-Za-z0-9_]+(\s*\+\s*(PP|\{PP\}))?|
                                      Auth\s*(Index|Handle):\s*([0-9]+|None)|
                                      (?:
                                       Auth\s*Role:\s*
                                       (Physical\s*Presence|[A-Z][A-Za-z]+)
                                       (\s*\+\s*(Physical\s*Presence|[A-Z][A-Za-z]+))*
                                      )").unwrap();
    for p in file.pages() {
        let p = p.unwrap();
        i += 1;
        match handle_page(&file, &p) {
            Ok(mut tables) => {
                if tables.is_empty() {
                    last_table.replace(None);
                }
                let mut is_first_in_page = true;
                while let Some(table) = tables.pop() {
                    let caption = table.captions.collect();
                    let captures = re_table_caption.captures(&caption);
                    let is_new_table = match captures {
                        Some(captures) => {
                            last_table.replace(None);
                            if !is_first {
                                println!();
                            } else {
                                is_first = false;
                            }

                            let table_no = captures.name("TABLENO").unwrap().as_str();

                            let subject = if let Some(type_definition) = captures.name("TYPE") {
                                table_type = TableType::TypeDef;
                                type_definition.as_str()
                            } else if let Some(command) = captures.name("COMMAND") {
                                table_type = TableType::CommandDef;
                                command.as_str()
                            } else if let Some(response) = captures.name("RESPONSE") {
                                table_type = TableType::ResponseDef;
                                response.as_str()
                            } else {
                                table_type = TableType::Defines;
                                let defines = captures.name("DEFINES").unwrap();
                                defines.as_str()
                            };

                            match cli.source_document_name.as_ref() {
                                Some(source_document_name) => {
                                    println!("BEGINTABLE \"{}, page {}, table {}\" {}",
                                             source_document_name, i, table_no, subject);
                                },
                                None => {
                                    println!("BEGINTABLE \"page {}, table {}\" {}", i, table_no, subject);
                                },
                            };

                            true
                        },
                        None => {
                            if !is_first_in_page || !last_table.can_continue_with(&table) {
                                last_table.replace(None);
                                continue;
                            }

                            false
                        },
                    };
                    is_first_in_page = false;


                    let last_col_header = table[(0, table.columns() - 1)].texts.collect();
                    match table_type {
                        TableType::TypeDef => {
                            let last_col_is_comments =
                                last_col_header.contains("Comments") || last_col_header.contains("Description")
                                || last_col_header.contains("Definition") || last_col_header.contains("Requirements")
                                || last_col_header.contains("Meaning");
                            for r in (!is_new_table as usize)..table.rows() {
                                for c in 0..(table.columns() - last_col_is_comments as usize) {
                                    if c != 0 {
                                        print!(";");
                                    }
                                    print!("{}", table[(r, c)].texts.collect().trim());
                                }
                                println!();
                            }
                        },
                        TableType::CommandDef => {
                            let last_col_is_descr = last_col_header.contains("Description");
                            let next_to_last_is_name =
                                table.columns() >= 2 &&
                                table[(0, table.columns() - 2)].texts.collect().contains("Name");
                            if is_new_table {
                                for c in 0..table.columns() {
                                    if c != 0 {
                                        print!(";");
                                    }
                                    print!("{}", table[(0, c)].texts.collect().trim());
                                }
                                println!();
                            }

                            let mut in_handle_area = false;
                            for r in 1..table.rows() {
                                let mut name = String::new();
                                for c in 0..(table.columns() - last_col_is_descr as usize) {
                                    if c != 0 {
                                        print!(";");
                                    }
                                    let field = table[(r, c)].texts.collect();
                                    if next_to_last_is_name && c == table.columns() - 2 {
                                        name = String::from_str(field.trim()).unwrap();
                                    }
                                    print!("{}", field.trim());
                                }

                                if last_col_is_descr {
                                    print!(";");
                                    if next_to_last_is_name {
                                        let descr = table[(r, table.columns() - 1)].texts.collect();
                                        if name == "tag" {
                                            let mut is_first = true;
                                            for cc in re_tag_descr.captures_iter(&descr) {
                                                if !is_first {
                                                    print!(", ");
                                                } else {
                                                    is_first = false;
                                                }
                                                print!("{}", &cc[0]);
                                            }
                                        } else if name == "commandCode" {
                                            let mut is_first = true;
                                            for cc in re_cc_descr.captures_iter(&descr) {
                                                if !is_first {
                                                    print!(", ");
                                                } else {
                                                    is_first = false;
                                                }
                                                print!("{}", &cc[0]);
                                            }
                                        } else if in_handle_area {
                                            let mut is_first = true;
                                            for cc in re_handle_descr.captures_iter(&descr) {
                                                if !is_first {
                                                    print!(", ");
                                                } else {
                                                    is_first = false;
                                                }
                                                print!("{}", &cc[0]);
                                            }
                                        }
                                    }
                                }
                                println!();

                                if table.is_special_sep_row(r + 1) {
                                    println!("HANDLE_AREA");
                                    in_handle_area = true;
                                } else if table.is_bold_sep_row(r + 1) {
                                    println!("PARAM_AREA");
                                    in_handle_area = false;
                                }
                            }
                        },
                        TableType::ResponseDef => {
                            let last_col_is_descr = last_col_header.contains("Description");
                            for r in (!is_new_table as usize)..table.rows() {
                                for c in 0..(table.columns() - last_col_is_descr as usize) {
                                    if c != 0 {
                                        print!(";");
                                    }
                                    print!("{}", table[(r, c)].texts.collect().trim());
                                }
                                println!();

                                if table.is_special_sep_row(r + 1) {
                                    println!("HANDLE_AREA");
                                } else if table.is_bold_sep_row(r + 1) {
                                    println!("PARAM_AREA");
                                }
                            }
                        },
                        TableType::Defines => {
                            let last_col_is_descr = last_col_header.contains("Description")
                                || last_col_header.contains("Comments");
                            for r in (!is_new_table as usize)..table.rows() {
                                for c in 0..(table.columns() - last_col_is_descr as usize) {
                                    if c != 0 {
                                        print!(";");
                                    }
                                    print!("{}", table[(r, c)].texts.collect().trim());
                                }
                                println!();
                            }
                        },
                        TableType::Unknown  => {},
                    }

                    if is_new_table {
                        last_table.replace(Some(table));
                    } else {
                        last_table.continue_with(table);
                    }
                }
            },
            Err(msg) => println!("Failed to process page {}: {}", i, msg),
        };
        // break;
    }
    last_table.replace(None);
}

#[derive(PartialEq, Clone, Copy, Debug)]
struct Coordinate {
    v: f32,
}

impl Eq for Coordinate {}


impl Coordinate {
    fn new(v: f32) -> Self {
        Self{v}
    }

    fn err_lower_margin(&self) -> Coordinate {
        Self::new(self.v - 1.)
    }

    fn err_upper_margin(&self) -> Coordinate {
        Self::new(self.v + 1.)
    }
}

impl TryFrom<&pdf::primitive::Primitive> for Coordinate {
    type Error = &'static str;

    fn try_from(v: &pdf::primitive::Primitive) -> Result<Self, Self::Error> {
        match v {
            pdf::primitive::Primitive::Number(v) => Ok(Coordinate::new(*v)),
            pdf::primitive::Primitive::Integer(v) => Ok(Coordinate::new(*v as f32)),
            _ => Err("Non-number primitive value specified for coordinate operand"),
        }
    }
}

impl PartialOrd for Coordinate {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.v.partial_cmp(&other.v)
    }
}

impl Ord for Coordinate {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl fmt::Display for Coordinate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:.2}", self.v)
    }
}

#[derive(Clone, Copy, Debug)]
struct Distance {
    d: f32,
}

impl Distance {
    fn new(d: f32) -> Self {
        Self{d}
    }
}

impl TryFrom<&pdf::primitive::Primitive> for Distance {
    type Error = &'static str;

    fn try_from(v: &pdf::primitive::Primitive) -> Result<Self, Self::Error> {
        match v {
            pdf::primitive::Primitive::Number(v) => Ok(Distance::new(*v)),
            pdf::primitive::Primitive::Integer(v) => Ok(Distance::new(*v as f32)),
            _ => Err("Non-number primitive value specified for coordinate operand"),
        }
    }
}

impl Add<Distance> for Coordinate {
    type Output = Coordinate;

    fn add(self, rhs: Distance) -> Self::Output {
        Coordinate::new(self.v + rhs.d)
    }
}

impl Sub<Distance> for Coordinate {
    type Output = Coordinate;

    fn sub(self, rhs: Distance) -> Self::Output {
        Coordinate::new(self.v - rhs.d)
    }
}

#[derive(Clone, Copy, Debug)]
struct CoordinateWithErr {
    lb: Coordinate,
    ub: Coordinate,
}

impl CoordinateWithErr {
    fn new(v: Coordinate) -> Self {
        Self{lb: v, ub: v}
    }

    fn err_lower_margin(&self) -> Coordinate {
        cmp::min(self.lb.err_lower_margin(), self.ub.err_lower_margin())
    }

    fn err_upper_margin(&self) -> Coordinate {
        cmp::max(self.lb.err_upper_margin(), self.ub.err_upper_margin())
    }

    fn fuse(&self, other: &CoordinateWithErr) -> Self {
        let lb = if self.lb <= other.lb {self.lb} else {other.lb};
        let ub = if self.ub >= other.ub {self.ub} else {other.ub};
        Self{lb, ub}
    }
}

impl PartialEq for CoordinateWithErr {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other).unwrap() == cmp::Ordering::Equal
    }
}

impl Eq for CoordinateWithErr {}

impl PartialOrd for CoordinateWithErr {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self.err_upper_margin() < other.err_lower_margin() {
            Some(cmp::Ordering::Less)
        } else if other.err_upper_margin() < self.err_lower_margin() {
            Some(cmp::Ordering::Greater)
        } else {
            Some(cmp::Ordering::Equal)
        }
    }
}

impl PartialEq<Coordinate> for CoordinateWithErr {
    fn eq(&self, other: &Coordinate) -> bool {
        self.partial_cmp(other).unwrap() == cmp::Ordering::Equal
    }
}

impl PartialOrd<Coordinate> for CoordinateWithErr {
    fn partial_cmp(&self, other: &Coordinate) -> Option<cmp::Ordering> {
        if self.err_upper_margin() < *other {
            Some(cmp::Ordering::Less)
        } else if *other < self.err_lower_margin() {
            Some(cmp::Ordering::Greater)
        } else {
            Some(cmp::Ordering::Equal)
        }
    }
}

impl Ord for CoordinateWithErr {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl fmt::Display for CoordinateWithErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.lb == self.ub {
            write!(f, "{}", self.lb)
        } else {
            write!(f, "{}:{}", self.lb, self.ub)
        }
    }
}

struct OrientedSegments {
    segments: AVLTree<CoordinateWithErr, IntervalTree<CoordinateWithErr, ()>>,
}

impl OrientedSegments {
    fn new() -> Self {
        Self{segments: AVLTree::new()}
    }

    fn insert_interval_one(intervals: &mut IntervalTree<CoordinateWithErr, ()>,
                           mut ib: CoordinateWithErr, mut ie: CoordinateWithErr) {
        let fuse_range_lb = CoordinateWithErr::new(ib.lb - Distance::new(5.));
        let fuse_range_ub = CoordinateWithErr::new(ie.ub + Distance::new(5.));
        while let Some(i0) = intervals.iter(Some(IntervalBound::Inclusive(fuse_range_lb)),
                                            Some(IntervalBound::Inclusive(fuse_range_ub))).next()
                                         .map(|(i0, ())| *i0) {
            let (i0, ()) = intervals.delete(&i0, &|_, _| true).unwrap();
            let i0b = i0.get_lb().unwrap();
            let i0e = i0.get_ub().unwrap();
            ib = if ib == *i0b {ib.fuse(i0b)} else {cmp::min(ib, *i0b)};
            ie = if ie == *i0e {ie.fuse(i0e)} else {cmp::max(ie, *i0e)};
        }
        intervals.insert(Interval::new(IntervalBound::Inclusive(ib),
                                       IntervalBound::Inclusive(ie)),
                         ());
    }

    fn insert(&mut self, u: Coordinate, u_fuse_distance: Distance, mut ib: Coordinate, mut ie: Coordinate) {
        match ib.cmp(&ie) {
            cmp::Ordering::Equal => return,
            cmp::Ordering::Less => (),
            cmp::Ordering::Greater => swap(&mut ib, &mut ie)
        }

        let ib = CoordinateWithErr::new(ib);
        let ie = CoordinateWithErr::new(ie);
        if ib >= ie {
            return;
        }

        let mut u = CoordinateWithErr::new(u);
        let mut intervals = IntervalTree::new();
        while let Some(u0) = {self.segments.iter(Some(CoordinateWithErr::new(u.lb - u_fuse_distance)),
                                                 Some(CoordinateWithErr::new(u.ub + u_fuse_distance)))
                              .next().map(|(u0, _)| *u0)} {
            let (u0, mut intervals0) = self.segments.delete(&u0, &|_, _| true).unwrap();
            u = u.fuse(&u0);
           swap(&mut intervals0, &mut intervals);

            for (i0, ()) in intervals0.iter(None, None) {
                Self::insert_interval_one(&mut intervals,
                                          *i0.get_lb().unwrap(),
                                          *i0.get_ub().unwrap());
            }
        };

        Self::insert_interval_one(&mut intervals, ib, ie);

        self.segments.insert(u, intervals);
    }
}

struct OrientedSegmentGroups {
    groups: IntervalTree<CoordinateWithErr, AVLTree<CoordinateWithErr, ()>>,
}

impl OrientedSegmentGroups {
}

impl From<OrientedSegments> for OrientedSegmentGroups {
    fn from(segments: OrientedSegments) -> Self {
        let mut groups: IntervalTree<CoordinateWithErr, AVLTree<CoordinateWithErr, ()>> = IntervalTree::new();

        for (u, segments) in segments.segments.iter(None::<CoordinateWithErr>, None::<CoordinateWithErr>) {
            for (segment, ()) in segments.iter(None::<IntervalBound::<CoordinateWithErr>>,
                                               None::<IntervalBound::<CoordinateWithErr>>) {
                // Ignore unreasonably short segments.
                if segment.get_lb().unwrap().lb + Distance::new(5.) >= segment.get_ub().unwrap().ub {
                    continue;
                }

                if let Some(us) =
                    {groups.iter_mut(Some(*segment.get_lb()), Some(*segment.get_ub()))
                     .find(|(other_segment, _)| {other_segment.get_lb() == segment.get_lb()
                                                 && other_segment.get_ub() == segment.get_ub()})
                     .map(|(_, us)| us)}
                {
                    us.insert(*u, ());
                } else {
                    let mut us = AVLTree::new();
                    us.insert(*u, ());
                    groups.insert(*segment, us)
                }
            }
        }

        Self{groups}
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Point {
    x: Coordinate,
    y: Coordinate,
}

impl Point {
    fn new(p: &pdf::content::Point) -> Self {
        Self{x: Coordinate::new(p.x), y: Coordinate::new(p.y)}
    }

    fn new_from_xy(x: Coordinate, y: Coordinate) -> Self {
        Self{x, y}
    }
}

#[derive(Clone, Copy, Debug)]
enum PathPoint {
    Move(Point), // (destination point)
    Line(Point), // (end point)
    Bezier(Point, Point, Point), // (control point 1, control point 2, end point)
}

#[derive(Clone, Debug)]
struct Path {
    points: Vec<PathPoint>,
}

impl Path {
    fn new() -> Self {
        Self{points: Vec::new()}
    }

    fn is_empty(&self) -> bool {
        self.points.is_empty()
    }

    fn push(&mut self, p: PathPoint) -> Result<(), &'static str> {
        match &p {
            PathPoint::Move(_) => {
                match self.points.last() {
                    Some(PathPoint::Move(_)) => {
                        self.points.pop();
                    },
                    Some(PathPoint::Line(_)) => {
                        // This entry is strictly needed for auto-closing -- the 'm' op starts a new subpath.
                    },
                    Some(PathPoint::Bezier(_, _, _)) => {
                        // This entry is strictly needed for auto-closing -- the 'm' op starts a new subpath.
                    },
                    None => (),
                }
            },
            PathPoint::Line(n) => {
                match self.points.last() {
                    Some(PathPoint::Move(l)) => {
                        if *l == *n {
                            return Ok(());
                        }
                    },
                    Some(PathPoint::Line(l)) => {
                        if *l == *n {
                            return Ok(());
                        }
                    },
                    Some(PathPoint::Bezier(_, _, l)) => {
                        if *l == *n {
                            return Ok(());
                        }
                    },
                    None => {
                        return Err("Line path operation with no starting point");
                    },
                }
            },
            PathPoint::Bezier(_, _, n) => {
                match self.points.last() {
                    Some(PathPoint::Move(l)) => {
                        if *l == *n {
                            return Ok(());
                        }
                    },
                    Some(PathPoint::Line(l)) => {
                        if *l == *n {
                            return Ok(());
                        }
                    },
                    Some(PathPoint::Bezier(_, _, l)) => {
                        if *l == *n {
                            return Ok(());
                        }
                    },
                    None => {
                        return Err("Bezier path operation with no starting point");
                    },
                }
            },
        }
        self.points.push(p);
        Ok(())
    }

    fn close(&mut self) {
        match self.points.last() {
            None => return,
            Some(PathPoint::Move(_)) => return,
            Some(PathPoint::Line(_)) => (),
            Some(PathPoint::Bezier(_, _, _)) => (),
        };

        let mut i = self.points.iter().rev();
        i.next();
        let to = loop {
            match i.next().unwrap() {
                PathPoint::Move(p) => break p,
                PathPoint::Line(_) => (),
                PathPoint::Bezier(_, _, _) => (),
            }
        };
        self.push(PathPoint::Line(*to)).unwrap();
    }

    fn extract_oriented_segments(&self,
                                 horizontal: &mut OrientedSegments,
                                 vertical: &mut OrientedSegments) {
        let mut points = self.points.iter();
        let last_stop = points.next();
        let mut last_stop = match last_stop {
            Some(PathPoint::Move(first_stop)) => first_stop,
            _ => return,
        };

        for point in points {
            match point {
                PathPoint::Move(stop) => last_stop = stop,
                PathPoint::Line(stop) => {
                    if last_stop.y == stop.y {
                        let u =last_stop.y;
                        let mut ib = last_stop.x;
                        let mut ie = stop.x;
                        if ib > ie { swap(&mut ib, &mut ie); }
                        horizontal.insert(u, Distance::new(5.), ib, ie);

                    } else if last_stop.x == last_stop.x {
                        let u = last_stop.x;
                        let mut ib = last_stop.y;
                        let mut ie = stop.y;
                        if ib > ie { swap(&mut ib, &mut ie); }
                        vertical.insert(u, Distance::new(5.), ib, ie);

                    }
                    last_stop = stop;
                },
                PathPoint::Bezier(_, _, stop) => last_stop = stop,
            };
        }
    }
}


#[derive(Clone, Debug)]
struct Text {
    point: Point,
    text: String,
}

impl Text {
    fn new(point: Point) -> Self {
        Self{point, text: String::new()}
    }

    fn append(&mut self, text: &str) {
        self.text.push_str(text);
    }
}

struct OrderedTexts {
   texts: Vec<Text>,
}

impl OrderedTexts {
    fn new() -> Self {
        Self{texts: Vec::new()}
    }

    fn insert(&mut self, text: Text) {
        let mut i = self.texts.len();
        let new_x = &text.point.x;
        let new_y = CoordinateWithErr::new(text.point.y);
        for j in 0..self.texts.len() {
            let cur = &self.texts[j];
            let cur_x = &cur.point.x;
            let cur_y = CoordinateWithErr::new(cur.point.y);
            if new_y > cur_y || (new_y == cur_y && new_x < cur_x) {
                i = j;
                break;
            }
        }

        self.texts.insert(i, text);
    }

    fn collect(&self) -> String {
        let mut s = String::new();
        for t in &self.texts {
            s.push_str(&t.text);
        }
        s
    }
}

impl AsRef<[Text]> for OrderedTexts {
    fn as_ref(&self) -> &[Text] {
        self.texts.as_ref()
    }
}

struct TableCell {
    texts: OrderedTexts,
}

impl TableCell {
    fn new() -> Self {
        Self{texts: OrderedTexts::new()}
    }
}

struct Table {
    xstops: Vec<CoordinateWithErr>,
    ystops: Vec<CoordinateWithErr>,
    cells: Vec<Vec<TableCell>>,
    captions: OrderedTexts,
    special_row_indices: Vec<usize>,
}

impl Table {
    fn new(xstops: Vec<CoordinateWithErr>,
           ystops: Vec<CoordinateWithErr>,
           special_y_indices: Vec<usize>) -> Self {
        if xstops.is_empty() && ystops.is_empty() {
            return Self{xstops, ystops, cells: Vec::new(), captions: OrderedTexts::new(),
                        special_row_indices: Vec::new()};
        }

        assert!(xstops.len() >= 2);
        assert!(ystops.len() >= 2);
        let columns = xstops.len() - 1;
        let rows = ystops.len() - 1;
        let mut cells = Vec::new();
        cells.resize_with(rows, || {
            let mut row = Vec::new();
            row.resize_with(columns, TableCell::new);
            row
        });
        let special_row_indices = Vec::from_iter(special_y_indices.iter().rev().map(|i| rows - i));
        Self{xstops, ystops, cells, captions: OrderedTexts::new(), special_row_indices}
    }

    fn horizontal_extent(&self) -> (&CoordinateWithErr, &CoordinateWithErr) {
        (self.xstops.first().unwrap(), self.xstops.last().unwrap())
    }

    fn vertical_extent(&self) -> (&CoordinateWithErr, &CoordinateWithErr) {
        (self.ystops.first().unwrap(), self.ystops.last().unwrap())
    }

    fn columns(&self) -> usize {
        self.xstops.len() - 1
    }

    fn rows(&self) -> usize {
        self.ystops.len() - 1
    }

    fn point_to_cell_mut(&mut self, p: &Point) -> Option<&mut TableCell> {
        let mut c = 0;
        for x in &self.xstops {
            if *x > p.x {
                break;
            }
            c += 1;
        }
        if c == 0 || c == self.xstops.len() {
            return None;
        }

        let mut r = 0;
        for y in &self.ystops {
            if *y > p.y {
                break;
            }
            r += 1;
        }
        if r == 0 || r == self.ystops.len() {
            return None;
        }

        r = self.rows() - r;
        Some(&mut self[(r, c - 1)])
    }

    fn is_special_sep_row(&self, r: usize) -> bool {
        if r > self.rows() {
            false
        } else {
            self.special_row_indices.iter().any(|sr| *sr == r)
        }
    }

    fn is_bold_sep_row(&self, r: usize) -> bool {
        let ystop = self.ystops[self.rows() - r];
        ystop.ub.v - ystop.lb.v >= 3.
    }

}

impl Index<(usize, usize)> for Table {
    type Output = TableCell;

    fn index(&self, index: (usize, usize)) -> &Self::Output {
        &self.cells[index.0][index.1]
    }
}

impl IndexMut<(usize, usize)> for Table {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
        &mut self.cells[index.0][index.1]
    }
}

fn check_path_empty(path: &Path) -> Result<(), &'static str> {
    if !path.is_empty() {
        Err("Incomplete PDF path definition")
    } else {
        Ok(())
    }
}

enum PDFTextDecoder {
    WinAnsiEncoding,
    ToUnicodeMap(pdf::font::ToUnicodeMap),
}

impl PDFTextDecoder {
    fn from_font(f: &Font, file: &impl pdf::object::Resolve) -> Option<Self> {
        if let Some(enc) = f.encoding() {
            if let pdf::encoding::BaseEncoding::IdentityH = enc.base {
                if enc.differences.is_empty() {
                    let tounicodemap = f.to_unicode(file);
                    if let Some(tounicodemap) = tounicodemap {
                        match tounicodemap {
                            Ok(tounicodemap) => return Some(Self::ToUnicodeMap(tounicodemap)),
                            Err(_) => return None,
                        }
                    }
                }
            }
        }

        match f.subtype {
            pdf::font::FontType::Type1 | pdf::font::FontType::Type3 | pdf::font::FontType::TrueType => {
                if let Some(enc) = f.encoding() {
                    if let pdf::encoding::BaseEncoding::WinAnsiEncoding = enc.base {
                        if enc.differences.is_empty() {
                            return Some(Self::WinAnsiEncoding);
                        }
                    }
                }
            },
            _ => (),
        };
        None
    }

    fn decode(&self, s: &pdf::primitive::PdfString) -> Result<String, &'static str> {
        match self {
            Self::WinAnsiEncoding => {
                let mut r: String = String::new();
                for c in {s.data.iter().map(|b| {
                    match *b {
                        0x80u8 => Ok('\u{20ac}'),
                        0x81u8 => Err("Encountered unallocated character code"),
                        0x82u8 => Ok('\u{201a}'),
                        0x83u8 => Ok('\u{0192}'),
                        0x84u8 => Ok('\u{201e}'),
                        0x85u8 => Ok('\u{2026}'),
                        0x86u8 => Ok('\u{2020}'),
                        0x87u8 => Ok('\u{2021}'),
                        0x88u8 => Ok('\u{02c6}'),
                        0x89u8 => Ok('\u{2030}'),
                        0x8au8 => Ok('\u{0160}'),
                        0x8bu8 => Ok('\u{2039}'),
                        0x8cu8 => Ok('\u{0152}'),
                        0x8du8 => Err("Encountered unallocated character code"),
                        0x8eu8 => Ok('\u{017d}'),
                        0x8fu8 => Err("Encountered unallocated character code"),
                        0x90u8 => Err("Encountered unallocated character code"),
                        0x91u8 => Ok('\u{2018}'),
                        0x92u8 => Ok('\u{2019}'),
                        0x93u8 => Ok('\u{201c}'),
                        0x94u8 => Ok('\u{201d}'),
                        0x95u8 => Ok('\u{2022}'),
                        0x96u8 => Ok('\u{2013}'),
                        0x97u8 => Ok('\u{2014}'),
                        0x98u8 => Ok('\u{02dc}'),
                        0x99u8 => Ok('\u{2122}'),
                        0x9au8 => Ok('\u{0161}'),
                        0x9bu8 => Ok('\u{203a}'),
                        0x9cu8 => Ok('\u{0153}'),
                        0x9du8 => Err("Encountered unallocated character code"),
                        0x9eu8 => Ok('\u{017e}'),
                        0x9fu8 => Ok('\u{0178}'),
                        _ => Ok(char::from(*b)),
                    }
                })} {
                    match c {
                        Ok(c) => r.push(c),
                        Err(msg) => return Err(msg),
                    }
                }
                Ok(r)
            },
            Self::ToUnicodeMap(map) => {
                let mut r: String = String::new();
                for c in {s.data.chunks(2).map(|b| {
                    let b = ((b[0] as u16) << 8) + (b[1] as u16);
                    match map.get(b) {
                        Some(c) => Ok(c),
                        None => Err("Encountered unallocated character code"),
                    }
                })} {
                    match c {
                        Ok(c) => r.push_str(c),
                        Err(msg) => return Err(msg),
                    }
                }
                Ok(r)
            },
        }
    }
}

fn tj_string_operand_to_string(operand: &pdf::primitive::PdfString,
                               decoder: &Option<&PDFTextDecoder>) -> Result<String, &'static str> {
    let decoder = match decoder {
        None => return Err("Attempting to decode text string without active decoder set"),
        Some(decoder) => decoder,
    };

    decoder.decode(operand)
}

fn tj_array_operand_to_string(operand: &[pdf::content::TextDrawAdjusted],
                              decoder: &Option<&PDFTextDecoder>) -> Result<String, &'static str> {
    let mut s = String::new();
    let decoder = match decoder {
        None => return Err("Attempting to decode text string without active decoder set"),
        Some(decoder) => decoder,
    };
    for t in operand.iter().filter_map(|e| match e { pdf::content::TextDrawAdjusted::Text(t) => Some(t), _ => None, }) {
        let t = decoder.decode(t)?;
        s = s + &t;
    }
    Ok(s)
}

#[derive(Debug)]
struct PendingText {
    pending: Text,
}

impl PendingText {
    fn new() -> Self {
        Self{pending: Text::new(Point::new_from_xy(Coordinate::new(0.), Coordinate::new(0.)))}
    }

    fn append(&mut self, text: &str) {
        self.pending.append(text);
    }

    fn move_displacement(&mut self, point: Point) -> Option<Text> {
        let p = mem::replace(&mut self.pending, Text::new(point));
        if !p.text.is_empty() {
            Some(p)
        } else {
            None
        }
    }

    fn reset(&mut self) -> Option<Text> {
        self.move_displacement(Point::new_from_xy(Coordinate::new(0.), Coordinate::new(0.)))
    }
}

fn handle_page(file: &File<Vec<u8>>, p: &Page) -> Result<Vec<Table>, &'static str> {
    let contents = match &p.contents {
        Some(c) => c,
        None => return Ok(Vec::new()),
    };

    // Ad-hoc algorithm for table reconstruction
    // 1.) Extract horizontal and vertical lines from the PDF drawing operations.
    // 2.) Group the families of horizontal and vertical lines into families of
    //     identical extent each.  This needs an interval tree?
    // 3.) For each pair of horizontal and vertical family:
    //     - Remove lines from the front and back of each family, which do not intersect with any
    //       of the lines in the other family. The result is lines intersecting with all the remaining
    //       lines in the other family. Per geometry.
    //     - The result is a table candidate.
    // 4.) A family can be part of more than one table candidate.
    //     If the candidates don't overlap, split them, they are separate.
    //     What remains are overlapping candidates. Do not support it for now
    //     and throw all overlapping tables away.
    // 5.) Finally sort the PDF text snippets into the corresponding tables' cell.
    let mut path = Path::new();
    let mut horizontal_segments = OrientedSegments::new();
    let mut vertical_segments = OrientedSegments::new();
    let mut special_horizontal_segments = OrientedSegments::new();

    #[derive(Clone)]
    struct Gs {
        cm: pdf::content::Matrix,
        font_name: Option<String>,
        text_line_matrix: pdf::content::Matrix,
        text_leading: f32,
    }

    impl Gs {
        fn text_pos(&self) -> Point {
            Point::new_from_xy(Coordinate::new(self.text_line_matrix.e),
                               Coordinate::new(self.text_line_matrix.f))
        }
    }

    let mut gs_stack: Vec<Gs> = Vec::new();
    gs_stack.push(Gs{
        cm: pdf::content::Matrix::default(), font_name: None,
        text_line_matrix: pdf::content::Matrix::default(), text_leading: 0.,
    });
    let text_decoders: Cell<HashMap<String, PDFTextDecoder>> = Cell::new(HashMap::new());
    let mut current_text_decoder: Option<&PDFTextDecoder> = None;
    let mut pending_text = PendingText::new();
    let mut texts: Vec<Text> = Vec::new();
    let ops = match contents.operations(file) {
        Ok(ops) => ops,
        Err(_) => return Err("Failed to parse page contents"),
    };
    for op in &ops {
        match &op {
            // Path construction operators
            pdf::content::Op::MoveTo{p} => {
                path.push(PathPoint::Move(Point::new(p)))?;
            },
            pdf::content::Op::LineTo{p} => {
                path.push(PathPoint::Line(Point::new(p)))?;
            },
            pdf::content::Op::CurveTo{c1, c2, p} => {
                path.push(PathPoint::Bezier(Point::new(c1), Point::new(c2), Point::new(p)))?;
            },
            pdf::content::Op::Close => {
                path.close();
            },
            pdf::content::Op::Rect{rect} => {
                let x = Coordinate::new(rect.x);
                let y = Coordinate::new(rect.y);
                let w = Distance::new(rect.width);
                let h = Distance::new(rect.height);
                path.push(PathPoint::Move(Point::new_from_xy(x, y)))?;
                path.push(PathPoint::Line(Point::new_from_xy(x + w, y)))?;
                path.push(PathPoint::Line(Point::new_from_xy(x + w, y + h)))?;
                path.push(PathPoint::Line(Point::new_from_xy(x, y + h)))?;
                path.push(PathPoint::Line(Point::new_from_xy(x, y)))?;
            },
            pdf::content::Op::Stroke => {
                path.extract_oriented_segments(&mut horizontal_segments, &mut vertical_segments);
                path = Path::new();
            },
            pdf::content::Op::FillAndStroke{winding: _} | pdf::content::Op::Fill{winding: _} => {
                path.extract_oriented_segments(&mut horizontal_segments, &mut vertical_segments);
                path = Path::new();
            },
            pdf::content::Op::Clip{winding: _} => {
                // Modify clipping path. Kind of special in that they don't "end" the current path object.
            },
            pdf::content::Op::EndPath => {
                // No-op, "ends" the current path object.
                path = Path::new();
            },

            // Xobject operator potentially drawing an image.
            pdf::content::Op::XObject{name} => {
                let mut is_image = false;
                if let Some(res) = &p.resources {
                    if let Some(xo) = res.xobjects.get(name) {
                        if let Ok(xo) = <pdf::file::File<Vec<u8>> as pdf::object::Resolve>::get(file, *xo) {
                            if let pdf::object::XObject::Image(_) = &*xo {
                                is_image = true;
                            }
                        }
                    }
                }
                if !is_image {
                    continue;
                }

                // The image gets mapped to user coordinates (0,0) - (1,1),
                // which then get transformed via cm. If the image's height is
                // smallish, it could be part of a specially drawn horizontal table
                // line (like the marker line representing the auth section in
                // commands).
                let cm = &gs_stack.last().unwrap().cm;
                let height = cm.b + cm.d;
                if height.abs() >= 5. {
                    continue;
                }

                let width = cm.a + cm.c;
                let x = Coordinate::new(cm.e);
                let y = Coordinate::new(cm.f);
                special_horizontal_segments.insert(y, Distance::new(5.),
                                                   x, x + Distance::new(width));
                special_horizontal_segments.insert(y + Distance::new(height), Distance::new(5.),
                                                   x, x + Distance::new(width));
            },

            // Graphics state operators
            pdf::content::Op::Save => {
                check_path_empty(&path)?;
                gs_stack.push(gs_stack.last().unwrap().clone());
            },
            pdf::content::Op::Restore => {
                check_path_empty(&path)?;
                gs_stack.pop();
                let gs = gs_stack.last().ok_or("GS pop on empty stack")?;
                match &gs.font_name {
                    Some(font_name) => {
                        current_text_decoder = unsafe{&*text_decoders.as_ptr()}.get(font_name);
                    },
                    None => {
                        current_text_decoder = None;
                    },
                };
            },
            pdf::content::Op::Transform{matrix} => {
                gs_stack.last_mut().unwrap().cm = *matrix;
            },

            // Text operators
            pdf::content::Op::BeginText => {
                check_path_empty(&path)?;
                let gs = gs_stack.last_mut().unwrap();
                gs.text_line_matrix = pdf::content::Matrix::default();
                pending_text.pending.point = gs.text_pos();
            },
            pdf::content::Op::EndText => {
                check_path_empty(&path)?;
                if let Some(t) = pending_text.reset() {
                    texts.push(t);
                }
            },
            pdf::content::Op::MoveTextPosition{translation} => {
                let gs = gs_stack.last_mut().unwrap();

                gs.text_line_matrix.e += translation.x * gs.text_line_matrix.a +
                    translation.y * gs.text_line_matrix.c;
                gs.text_line_matrix.f += translation.x * gs.text_line_matrix.b +
                    translation.y * gs.text_line_matrix.d;

                if let Some(t) = pending_text.move_displacement(gs.text_pos()) {
                    texts.push(t);
                }
            },
            pdf::content::Op::SetTextMatrix{matrix} => {
                let gs = gs_stack.last_mut().unwrap();
                gs.text_line_matrix = *matrix;
                if let Some(t) = pending_text.move_displacement(gs.text_pos()) {
                    texts.push(t);
                }
            },
            pdf::content::Op::Leading{leading} => {
                gs_stack.last_mut().unwrap().text_leading = *leading;
            },
            pdf::content::Op::TextNewline => {
                check_path_empty(&path)?;
                let gs = gs_stack.last_mut().unwrap();

                gs.text_line_matrix.e += -gs.text_leading * gs.text_line_matrix.c;
                gs.text_line_matrix.f += -gs.text_leading * gs.text_line_matrix.d;

                if let Some(t) = pending_text.move_displacement(gs.text_pos()) {
                    texts.push(t);
                }
            },

            pdf::content::Op::TextDraw{text} => {
                check_path_empty(&path)?;
                pending_text.append(&tj_string_operand_to_string(text, &current_text_decoder)?);
            },

            pdf::content::Op::TextDrawAdjusted{array} => {
                check_path_empty(&path)?;
                pending_text.append(&tj_array_operand_to_string(array, &current_text_decoder)?);
            }

            pdf::content::Op::TextFont{name, size: _} => {
                check_path_empty(&path)?;
                let name = <pdf::primitive::Name as Borrow::<str>>::borrow(name);
                gs_stack.last_mut().unwrap().font_name = Some(name.to_owned());
                current_text_decoder = None;
                match unsafe{&*text_decoders.as_ptr()}.get(name) {
                    Some(d) => current_text_decoder = Some(d),
                    None => {
                        if let Some(res) = &p.resources {
                            if let Some(f) = res.fonts.get(name) {
                                let decoder = PDFTextDecoder::from_font(f, file);
                                if let Some(decoder) = decoder {
                                    unsafe{&mut *text_decoders.as_ptr()}.insert(name.to_string(), decoder);
                                    current_text_decoder = Some(unsafe{&*text_decoders.as_ptr()}.get(name).unwrap());
                                }
                            }
                        }
                    },
                };
            },

            // Other operators
            _ => {
                check_path_empty(&path)?;
            },
        }
    }

    // for (y, segs) in horizontal_segments.segments.iter(None::<CoordinateWithErr>, None::<CoordinateWithErr>) {
    //     println!("H Segments @{}", y);
    //     for (i, _) in segs.iter(None, None) {
    //         println!("\t[{}, {}]", i.get_lb().unwrap(), i.get_ub().unwrap());
    //     }
    // }

    // for (x, segs) in vertical_segments.segments.iter(None::<CoordinateWithErr>, None::<CoordinateWithErr>) {
    //     println!("V Segments @{}", x);
    //     for (i, _) in segs.iter(None, None) {
    //         println!("\t[{}, {}]", i.get_lb().unwrap(), i.get_ub().unwrap());
    //     }
    // }

    let horizontal_groups: OrientedSegmentGroups = horizontal_segments.into();
    let vertical_groups: OrientedSegmentGroups = vertical_segments.into();
    // for (seg, ys) in horizontal_groups.groups.iter(None::<IntervalBound::<CoordinateWithErr>>,
    //                                                None::<IntervalBound::<CoordinateWithErr>>) {
    //     println!("H Segment group [{}, {}]", seg.get_lb().unwrap(), seg.get_ub().unwrap());
    //     for (y, _) in ys.iter(None::<CoordinateWithErr>, None::<CoordinateWithErr>) {
    //         println!("\t{}", y);
    //     }
    // }

    // for (seg, xs) in vertical_groups.groups.iter(None::<IntervalBound::<CoordinateWithErr>>,
    //                                             None::<IntervalBound::<CoordinateWithErr>>) {
    //     println!("V Segment group [{}, {}]", seg.get_lb().unwrap(), seg.get_ub().unwrap());
    //     for (x, _) in xs.iter(None::<CoordinateWithErr>, None::<CoordinateWithErr>) {
    //         println!("\t{}", x);
    //     }
    // }

    let mut tables: IntervalTree<CoordinateWithErr, Table> = IntervalTree::new();
    for (hseg, ys) in horizontal_groups.groups.iter(None::<IntervalBound::<CoordinateWithErr>>,
                                                    None::<IntervalBound::<CoordinateWithErr>>) {
        let ys = Vec::from_iter(ys.iter(None::<CoordinateWithErr>, None::<CoordinateWithErr>)
                                .map(|(y, _)| y));
        let xlb = hseg.get_lb().unwrap();
        let xub = hseg.get_ub().unwrap();
        for (vseg, xs) in vertical_groups.groups.iter(Some(IntervalBound::Inclusive(**ys.first().unwrap())),
                                                      Some(IntervalBound::Inclusive(**ys.last().unwrap()))) {
            let ylb = vseg.get_lb().unwrap();
            let yub = vseg.get_ub().unwrap();
            let mut ys = Vec::from_iter(ys.iter().map(|y| **y).filter(|y| {*y >= *ylb && *y <= *yub}));
            if ys.is_empty() {
                continue;
            }
            let mut xs = Vec::from_iter({xs.iter(None::<CoordinateWithErr>, None::<CoordinateWithErr>)
                                         .map(|(x, _)| *x).filter(|x| {*x >= *xlb && *x <= *xub})});
            if xs.is_empty() {
                continue;
            }

            // If the segment exceeds the stop range, add phantom stops to cover
            // the table bounds.
            if ys.first().unwrap() > ylb {
                ys.insert(0, *ylb);
            }
            if ys.last().unwrap() < yub {
                ys.push(*yub);
            }

            if xs.first().unwrap() > xlb {
                xs.insert(0, *xlb);
            }
            if xs.last().unwrap() < xub {
                xs.push(*xub);
            }

            if xs.len() <= 2 || ys.len() <= 2 {
                continue;
            }

            // Search for specially drawn horizontal segments in the covered range and
            // include them as special ystops.
            let mut special_ys: Vec<CoordinateWithErr> = Vec::new();
            for (y, hsegs) in special_horizontal_segments.segments.iter(Some(*ylb), Some(*yub)) {
                let mut total_overlap: f32 = 0.;
                for (hseg, _) in hsegs.iter(Some(IntervalBound::Inclusive(*xlb)),
                                            Some(IntervalBound::Inclusive(*xub))) {
                    let hseg_lb = hseg.get_lb().unwrap();
                    let hseg_ub = hseg.get_ub().unwrap();
                    if (hseg_lb < xlb && hseg_lb.ub + Distance::new(5.) < xlb.lb)
                        || (hseg_ub > xub && hseg_ub.lb - Distance::new(5.) > xub.ub){
                        continue;
                    }

                    let overlap_lb = if hseg_lb < xlb {xlb} else {hseg_lb};
                    let overlap_ub = if hseg_ub > xub {xub} else {hseg_ub};
                    let overlap = overlap_ub.lb.v - overlap_lb.ub.v;
                    if overlap > 0. {
                        total_overlap += overlap;
                    }
                }

                // If >= 80% of the table's horizontal extent is covered by some
                // specially drawn horizontal segments, their common y position
                // is considered a special ystop.
                let hext = xub.ub.v - xlb.lb.v;
                if total_overlap > hext || total_overlap / hext > 0.8 {
                    special_ys.push(*y);
                }
            }

            let mut special_y_indices: Vec<usize> = Vec::new();
            for special_y in special_ys.iter() {
                let mut i = ys.len();
                let mut duplicate = false;
                for (j, y) in ys.iter().enumerate() {
                    if y == special_y {
                        i = j;
                        duplicate = true;
                        break;
                    } else if y.lb >= special_y.ub {
                        i = j;
                        break;
                    }
                }
                if duplicate {
                    ys[i] = *special_y;
                } else {
                    ys.insert(i, *special_y);
                }
                special_y_indices.push(i);
            }

            // println!("Found Table");
            // for x in &xs {
            //     println!("\txstop={}", x);
            // }
            // for y in &ys {
            //     println!("\tystop={}", y);
            // }

            let table = Table::new(xs, ys, special_y_indices);
            let vext = table.vertical_extent();
            // Make sure the vertical extent key includes the region right above
            // the table for covering potential caption text positions.
            let lb = *vext.0;
            let ub = CoordinateWithErr::new(vext.1.lb + Distance::new(16.));
            tables.insert(Interval::new(IntervalBound::Inclusive(lb), IntervalBound::Inclusive(ub)), table);
        }
    }

    // Throw overlapping tables away -- multirow/multicolumn is not supported by now.
    let mut overlapping: Vec<(Interval<CoordinateWithErr>, *const Table)> = Vec::new();
    for table in tables.iter(None, None) {
        let key = table.0;
        let table = table.1;
        let vext = table.vertical_extent();

        for voverlapping in tables.iter(Some(IntervalBound::Inclusive(*vext.0)),
                                       Some(IntervalBound::Inclusive(*vext.1))) {
            let voverlapping = voverlapping.1;
            if ptr::eq(table as *const _, voverlapping as *const _) {
                continue
            }

            let hext = table.horizontal_extent();
            let voverlapping_hext = voverlapping.horizontal_extent();
            if hext.1 <= voverlapping_hext.0 || voverlapping_hext.1 <= hext.0 {
                continue;
            }

            overlapping.push((*key, table as *const _));
        }
    }

    while let Some(overlap) = overlapping.pop() {
        tables.delete(&overlap.0, &|_key, table| table as *const _ == overlap.1);
    }

    texts.reverse();
    while let Some(t) = texts.pop() {
        let y = CoordinateWithErr::new(t.point.y);
        for table in tables.iter_mut(Some(IntervalBound::Inclusive(y)),
                                     Some(IntervalBound::Inclusive(y))) {
            let table = table.1;
            let hext = table.horizontal_extent();
            if hext.0 > &t.point.x || hext.1 < &t.point.x {
                continue;
            }

            let vext = table.vertical_extent();
            if vext.1 < &t.point.y {
                table.captions.insert(t);
                break;
            }

            if let Some(cell) = table.point_to_cell_mut(&t.point) {
                cell.texts.insert(t);
                break;
            }
        }
    }

    let mut result = Vec::new();
    for table in tables.iter_mut(None, None) {
        let table = mem::replace(table.1, Table::new(Vec::new(), Vec::new(), Vec::new()));
        result.push(table);
    }

    Ok(result)
}
