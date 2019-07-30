import * as lib from './etc/lib.js';
import {cssVar} from './etc/css-var.js';

let DT_table;
let lv = 0;

var options = {
    bottom: '64px', // default: '32px'
    right: 'unset', // default: '32px'
    left: '32px', // default: 'unset'
    time: '0.5s', // default: '0.3s'
    mixColor: 'inherit', // default: '#fff'
    backgroundColor: 'inherit',  // default: '#fff'
    buttonColorDark: '#100f2c',  // default: '#100f2c'
    buttonColorLight: '#fff', // default: '#fff'
    saveInCookies: true, // default: true,
}
  
export const darkmode = new Darkmode(options);

const _DT_initComplete = function(dt) {
    return(lib._DT_initComplete(dt));
}

$(document).ready(() => {
    $('body').addClass('ms-Fabric').attr('dir', 'ltr');
    
    $(document).on('click','#inputDialog #fileInput input:text', function(){
        $($(this).parent()).find('label.input-group-btn').click();
    });

    $(document).on('click', '#fullProgram #fileInput input:text', function(){
        Shiny.setInputValue('reloadRequest', true);
    });

    $(document).on('click', '#fullProgram .navbar-brand', () => {
        if (lv == 5) {
            Shiny.setInputValue('sendHiddenMsg', true);
            lv = 0;
        } else lv++ ;  
    });

    $(document).on('click', '#defTable table.display.dataTable.no-footer tr', function(){
        if ($(this).parent().find('tr.selected').length > 0){
            $('#defTable #defTable_edit').removeClass('btn-disabled');
            $('#defTable #defTable_remove').removeClass('btn-disabled');
            $('#defTable #defTable_copy').removeClass('btn-disabled');
        } else{
            $('#defTable #defTable_edit').addClass('btn-disabled');
            $('#defTable #defTable_remove').addClass('btn-disabled');
            $('#defTable #defTable_copy').addClass('btn-disabled');
        }
    });

    $(document).on('click','.navbar-brand', () => darkmode.toggle());

});

Shiny.addCustomMessageHandler('sheetPicker_on', state => {
    cssVar.FileInputWidth = state ? '50%' : '100%'; 
});

Shiny.addCustomMessageHandler('dataReady', state => {
    $('#inputDialog .dialog').addClass('out');
});

Shiny.addCustomMessageHandler('isExcel', isExcel => {
    if (isExcel) $('#inputDialog #fileInput').animate({top: '0px'}, 500);
    else $('#inputDialog #fileInput').animate({top: 'inherit'}, 500);
});

Shiny.addCustomMessageHandler('dokidoki', doki => {
    if (doki) $('#inputDialog').addClass('doki-doki');
});

Shiny.addCustomMessageHandler('to_tmp', id => {
    $('#tmp').append($(id));
});

Shiny.addCustomMessageHandler('reloadRequest-ans', mess => {
    if (mess) history.go(0);
     else Shiny.setInputValue('reloadRequest', false);
});

Shiny.addCustomMessageHandler('set-input-value', mess => {
    console.log(mess);
   Shiny.setInputValue(mess.input, mess.value);
});

Shiny.addCustomMessageHandler('show', el => {
    $(el).show();
})

