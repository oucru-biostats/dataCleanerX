import * as lib from './etc/lib.js';
let DT_table;

const _DT_initComplete = function(dt) {
    return(lib._DT_initComplete(dt));
}
$(document).ready(() => {
    $('body').addClass('ms-Fabric');
    
    $(document).on('click','#inputDialog #fileInput input:text', function(){
        $($(this).parent()).find('label.input-group-btn').click();
    });

    $(document).on('click', '#fullProgram #fileInput input:text', function(){
        Shiny.setInputValue('reloadRequest', true);
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

