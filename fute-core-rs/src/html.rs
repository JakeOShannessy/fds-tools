use std::collections::HashMap;

#[derive(Clone,Debug)]
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

#[derive(Clone,Debug)]
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

#[derive(Clone,Debug)]
pub struct HtmlPage {
    pub sections: Vec<HtmlElement>,
}

impl HtmlPage {
    pub fn add(&mut self, section: HtmlElement) {
        self.sections.push(section)
    }
}

impl Html for HtmlPage {
    fn render(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        let mut page = HtmlElement::new("html".to_string());
        let css = include_str!("./summary.css");
        let mut head = HtmlElement::new("head".to_string());
        let mut style = HtmlElement::new("style".to_string());
        style.children.push(HtmlChild::String(css.to_string()));
        head.children.push(HtmlChild::Element(style));
        head.children.push(HtmlChild::String(format!(
            "<script type=\"text/javascript\">{}</script>",
            r#"
            function toggle_visibility(ev,el) {
                if (el.classList.contains("test")) {
                  el.classList.toggle("shown-test");
                  el.classList.toggle("hidden-test");
                  ev.stopPropagation();
                };
            }"#
        )));
        page.children.push(HtmlChild::Element(head));
        let mut body = HtmlElement::new("body".to_string());
        for section in &self.sections {
            body.children.push(HtmlChild::Element(section.clone()));
        }
        page.children.push(HtmlChild::Element(body));
        page.render(f)
    }
}
