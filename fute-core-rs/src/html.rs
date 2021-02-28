use std::collections::HashMap;

pub enum HtmlChild {
    String(String),
    Element(HtmlElement),
}

impl Html for HtmlChild {
    fn render(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Element(e) => e.render(f),
        }
    }
}

pub struct HtmlElement {
    pub tag: String,
    pub attributes: HashMap<String, String>,
    pub children: Vec<HtmlChild>,
}

impl HtmlElement {
    pub fn new(tag: String) -> Self {
        Self {
            tag,
            attributes: HashMap::new(),
            children: Vec::new(),
        }
    }
}

impl Html for HtmlElement {
    fn render(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "<{}", self.tag)?;
        for (key, value) in self.attributes.iter() {
            write!(f, " {}=\"{}\"", key, value)?;
        }
        write!(f, ">")?;
        for child in self.children.iter() {
            child.render(f)?;
        }
        write!(f, "</{}>", self.tag)
    }
}

pub trait Html {
    fn render(&self, f: &mut impl std::io::Write) -> std::io::Result<()>;
}

// A verbatim string.
impl Html for String {
    fn render(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "{}", self)
    }
}
