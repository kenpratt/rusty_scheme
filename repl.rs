extern crate libc;
use std::c_str::CString;

#[link(name = "readline")]
extern {
    fn readline(prompt: *const libc::c_char) -> *const libc::c_char;
}

fn prompt_for_input(prompt: &str) -> Option<String> {
    let prompt_c_str = prompt.to_c_str();
    unsafe {
        let raw = readline(prompt_c_str.as_ptr());
        let cs = CString::new(raw, true);
        if cs.is_null() {
            None
        } else {
            cs.as_str().map(|s| s.to_str())
        }
    }
}

pub fn start(prompt: &str, f: |String| -> Result<String, String>) {
    loop {
        match prompt_for_input(prompt) {
            Some(input) => {
                let result = f(input);
                println!("{}", result.unwrap_or_else(|e| e));
            },
            None => return
        };
    };
}