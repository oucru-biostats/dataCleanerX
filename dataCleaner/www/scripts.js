/* var declaration */

let cssVar = new CSSGlobalVariables();
let barSize = {barLeft: 0, barRight: 'auto'};

Shiny.addCustomMessageHandler('sheetPicker_on', state => {
    cssVar.FileInputWidth = state ? '50%' : '100%'; 
});

const show_full_ui = () => {
    Shiny.setInputValue('dataReady', 1);
};

const Shiny_setInputValue = (mess, value) => {
    Shiny.setInputValue(mess, value);
}