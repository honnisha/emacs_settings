use emacs::{defun, Env, Result, Value};
use scraper::{Html, Selector, ElementRef};
use regex::Regex;

emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done parsers loading!")
}

fn select_data(selector: &str, block: ElementRef, default: &str) -> String {
    let mut views = String::from(default);
    let views_element = block.select(&Selector::parse(selector).unwrap()).next();
    if !views_element.is_none() {
        views = views_element.unwrap().inner_html();
    }
    views
}

pub fn get_freelansim_links() -> Vec<Vec<String>> {
    let mut links_data: Vec<Vec<String>> = vec![];
    
    let url = "https://freelansim.ru/tasks";
    let mut resp = reqwest::get(url).unwrap();
    assert!(resp.status().is_success());
    
    let body = resp.text().unwrap();

    let fragment = Html::parse_document(&body);
    let selector = Selector::parse(r#"article[class="task task_list"]"#).unwrap();

    for block in fragment.select(&selector) {
        let mut info: Vec<String> = vec![];
        let title_block = block.select(&Selector::parse("a").unwrap()).next().unwrap();

        info.push(title_block.inner_html());

        info.push(String::from(title_block.value().attr("href").unwrap()));

        info.push(select_data(".params__responses i", block, "0"));
        info.push(select_data(".params__views i", block, "0"));
        info.push(select_data(".params__published-at span", block, "0"));

        let re = Regex::new(r#"<[a-zA-Z0-9 /=\\"]+>"#).unwrap();
        info.push(re.replace_all(&select_data(".count", block, "-"), "").to_string());

        links_data.push(info);
    }
    links_data
}

#[defun]
pub fn get_frelansim_env(env: &Env) -> Result<Value> {
    let mut values: Vec<Value> = vec![];

    for info in get_freelansim_links() {
        // values.push(env.list(("name", "test", "test2"))?);
        values.push(env.list((&info[0], &info[1], &info[2], &info[3], &info[4], &info[5]))?);
    }
    env.list(&values)
}
