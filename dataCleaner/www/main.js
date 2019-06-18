import * as lib from './etc/lib.js';
let DT_table;
let lv = 0;


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

