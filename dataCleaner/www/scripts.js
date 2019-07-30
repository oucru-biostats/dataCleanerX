/* var declaration */

const show_full_ui = () => {
    Shiny.setInputValue('dataReady', 1);
};

const Shiny_setInputValue = (mess, value) => {
    Shiny.setInputValue(mess, value);
}

