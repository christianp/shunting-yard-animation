import show_error from './show-error.mjs';
async function init_app() {
    const compilation_error = await show_error;
    if(compilation_error) {
        return;
    }
    const app = Elm.App.init({node: document.body, flags: {}});
}

init_app();
