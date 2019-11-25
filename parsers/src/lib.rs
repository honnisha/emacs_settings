use emacs::{defun, Env, Result, Value};

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done parsers loading!")
}

// Define a function callable by Lisp code.
#[defun(user_ptr)]
fn get_frelansim_data() -> Result<Vec<String>> {
    let mut vec = vec![];
    for i in 0..10 {
        vec.push(i.to_string());
    }
    Ok(vec)
}
