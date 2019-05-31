const _DT_initComplete = function(dt) { 
    DT_table = dt;  
    console.log('3');
    Shiny.setInputValue('dataReady', true);
    $('#data-table .dataTables_scroll .simplebar-content').scroll(function(){
    let left = $('#data-table .dataTables_scrollBody .simplebar-content').scrollLeft();
    $('#data-table .dataTables_scrollHead').animate({
        scrollLeft: left}, 5);
    });
    SimpleBar_init('#methodsNav .tab-pane');
    SimpleBar_init('#dictNav .tab-pane');
    cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
    cssVar.contentinnerHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active .simplebar-content .row').height() + 'px';

    const table = dt.api().table();

    // ready_state_init();

    // _init_methodsSection();
}

