use std::{
    cell::RefCell,
    convert::Infallible,
    env,
    fs::File,
    io::Read,
    rc::Rc,
    str::{FromStr, Lines},
};

#[derive(Debug)]
struct Node {
    id: String,
    children: Vec<Rc<RefCell<Node>>>,
    size: Option<usize>,
}

impl Node {
    pub fn new() -> Node {
        Node {
            id: String::from("/"),
            children: vec![],
            size: None,
        }
    }

    pub fn insert(&mut self, node: Rc<RefCell<Node>>) {
        self.children.push(node)
    }

    pub fn print_dfs(&self) {
        println!("{} {}", self.size.unwrap_or_else(|| 0), self.id);
        for child in self.children.iter() {
            child.borrow().print_dfs();
        }
    }

    pub fn sum_of_largest(&self, max: usize) -> usize {
        let mut sizes: Vec<usize> = vec![];
        self.sum_helper(&mut sizes);
        sizes.into_iter().filter(|s| s <= &max).sum()
    }

    fn sum_helper(&self, sizes: &mut Vec<usize>) -> usize {
        if self.children.len() == 0 {
            return self.size.expect("should have size");
        }
        let mut sum = 0;
        for child in self.children.iter() {
            sum += child.borrow().sum_helper(sizes);
        }
        sizes.push(sum);
        sum
    }

    pub fn find_smallest_dir(&self, min: usize) -> usize {
        let mut sizes: Vec<usize> = vec![];
        self.sum_helper(&mut sizes);
        sizes.sort_by(|a, b| b.cmp(a));
        let root_size = sizes.first().expect("there is a size");
        let free_space = 70000000 - root_size;
        let target_del_size = 30000000 - free_space;
        let mut filtered = sizes
            .into_iter()
            .filter(|s| s >= &target_del_size)
            .collect::<Vec<usize>>();
        filtered.sort();
        *filtered.first().expect("aoc input good")
    }
}

impl<'a> FromIterator<&'a str> for Node {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let size = iter.next().unwrap().parse::<usize>().ok();
        let id = iter.next().expect("Missing name");
        Node {
            id: id.to_string(),
            size,
            children: vec![],
        }
    }
}

impl FromStr for Node {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Node, Self::Err> {
        return Ok(s.split_whitespace().collect::<Node>());
    }
}

fn build_file_tree(
    cur_node: Rc<RefCell<Node>>,
    input: &mut Lines,
    stack: &mut Vec<Rc<RefCell<Node>>>,
) {
    let node = cur_node.clone();
    let line = input.next();
    if let None = line {
        return;
    }

    if line.unwrap().starts_with("$") {
        let line = line
            .unwrap()
            .split_whitespace()
            .take(3)
            .collect::<Vec<&str>>();
        match line[1] {
            "cd" => match line[2] {
                ".." => {
                    stack.pop().expect("aoc gave me good input");
                    let next_node = stack.last().expect("plz");
                    return build_file_tree(next_node.clone(), input, stack);
                }
                "/" => {
                    let next_node = stack.drain(..1).next().unwrap();
                    return build_file_tree(next_node, input, stack);
                }
                _ => {
                    let next_node = node
                        .borrow_mut()
                        .children
                        .clone()
                        .into_iter()
                        .find(|n| n.borrow().id == line[2])
                        .expect("couldn't find dir");
                    stack.push(next_node.clone());
                    return build_file_tree(next_node, input, stack);
                }
            },
            _ => (),
        };
    } else {
        let new_node: Node = line
            .expect("line exists")
            .parse::<Node>()
            .expect("line is valid");

        node.borrow_mut().insert(Rc::new(RefCell::new(new_node)));
    }

    build_file_tree(node, input, stack)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let mut lines = file_contents.lines().into_iter();
    let root_ref = Rc::new(RefCell::new(Node::new()));
    // The first line is cd just throw it out.
    lines.next().expect("Missing $ cd /");
    let mut stack = vec![root_ref.clone()];
    build_file_tree(root_ref.clone(), &mut lines, &mut stack);
    let root = root_ref.borrow();
    root.print_dfs();

    println!("{}", root.sum_of_largest(100000));
    println!("{}", root.find_smallest_dir(30000000))
}
