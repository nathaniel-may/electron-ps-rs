use neon::prelude::*;

fn get_path(mut cx: FunctionContext) -> JsResult<JsString> {
    if let Some(new_path) = rfd::FileDialog::new().pick_file() {
        Ok(cx.string(new_path.to_string_lossy()))
    } else {
        Ok(cx.string("No File Found"))
    }
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
    cx.export_function("pick_file", get_path)?;
    Ok(())
}
