use emacs::{defun, Env, Result, Value};
use scraper::{Html, Selector};

emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done parsers loading!")
}

pub fn get_freelansim_links() -> Vec<Vec<char>> {
    let mut links_data: Vec<Vec<char>> = vec![];
    
    let url = "https://freelansim.ru/tasks";
    let mut resp = reqwest::get(url).unwrap();
    assert!(resp.status().is_success());
    
    let body = resp.text().unwrap();

    let fragment = Html::parse_document(&body);
    let selector = Selector::parse("article/div/header/div[1]/a").unwrap();

    for element in fragment.select(&selector) {
        assert_eq!("li", element.value().name());
    }
    links_data
}

#[defun]
pub fn get_frelansim_env(env: &Env) -> Result<Value> {
    let mut values: Vec<Value> = vec![];

    values.push(env.list(("name", "test", "test2"))?);
    values.push(env.list(("2", "2", "2"))?);
    env.list(&values)
}
