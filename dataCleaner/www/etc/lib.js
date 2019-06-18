export const set_inputDT = function(table){
    table.cells().every(function(i, j, tab, cell) {
        var $this = $(this.node());
        if ($this.find('input').length > 0){
          $this.find('input').addClass('form-control');
        }
      });
}

export const tagParse = function(table){
    table.cells().every(function(i, j, tab, cell){
        var $this = $(this.node());
        tagList = ['wrong','corrected', 'navalue', 'loner'];
        for (i = 0; i < tagList.length; i++) {
            tag = tagList[i];
            
            if ($this.find(tag).length > 0){
                content = $this.find(tag).html();
                $this.html(content);
                $this.addClass(tag + '-data');
            }
        }
    });
};

export const SimpleBar_init = (elList) => {
    $(document).find(elList).each(function(idx, el) {
        new SimpleBar(el);
        $(el).css('overflow','unset');
    });
}

export const responsiveText = function(){
    cssVar.grandTabTop = ($('#dataset').position().top + $('#dataset').outerHeight()) + 'px';
    if ($(window).width() < 768){
        $('.paginate_button.previous').html('◄');
        $('.paginate_button.next').html('►');
    } else {
        $('.paginate_button.previous').html('Previous');
        $('.paginate_button.next').html('Next');
    }    
}

export const _init_methodsSection = () => {
    // if ($(window).width() < 768){
    //     $('#methodsNav').addClass('smallMedia', 300);
    //     $('#methodsToggle').addClass('smallMedia', 300);
    //     $('#methodsNav').find('a').map((idx, a) => $(a).html(nav_title.short_form[idx]));
    // } else {
    //     $('#methodsNav').removeClass('smallMedia', 300);
    //     $('#methodsToggle').removeClass('smallMedia', 300);
    //     $('#methodsNav').find('a').map((idx, a) => $(a).html(nav_title.long_form[idx]));
    // }   
}

export const _DT_highlight = (dt, rows, col) => {    
    rows.map(row => {
        $(dt.row(row).node()).addClass('highlight', 300);
        $(dt.cell(row, col).node()).addClass('highlight', 300);
    })
}

export async function get_highlightCol(dt, highlightVar){
    headers = $(dt.header()).find('th').map(function() {return this.innerText});
    highlightCol = [];
    headers.map((idx, header) => {
        if (highlightVar.includes(header)) highlightCol.push(idx);
    });
    
    return highlightCol;
}

export async function get_highlightRow(dt, highlightIdx){
    idCol = dt.column(0).data();
    highlightRow = [];
    idCol.map((id, idx) => {
        if (highlightIdx.includes(Number(id))) highlightRow.push(idx);
    });

    return highlightRow;
}

export async function get_highlightCoord(dt, highlightChkResVal){
    highlightVar = Object.keys(highlightChkResVal);

    highlightCol = await get_highlightCol(dt, highlightVar);
    highlightCoord = highlightVar.map(async function(thisVar, idx){
        highlightIdx = highlightChkResVal[thisVar];
        thisCol = highlightCol[idx];
        highlightRow = await get_highlightRow(dt, highlightIdx);
        return {row: highlightRow, col: thisCol};
    })

    if (highlightCoord.length) return highlightCoord;
}

export const _DT_callback = function(dt) {
    SimpleBar_init('#dataset .dataTables_scrollBody');
    table = dt.api().table();
    if (Object.values(highlightChkResVal).length)
     get_highlightCoord(table, highlightChkResVal)
     .then(coord => {
        coord.map(promise => promise.then(c => {
            if (c.row && c.col)  _DT_highlight(table, c.row, c.col);
        }));
     });
    
    // _DT_highlight(table, indexes);
    // $(table.cell({rowSelector: column(), columnSelector: 'BIRTHMON:name'}).node()).addClass('highlight');
    // set_inputDT(table); 
    // tagParse(table);

    /* Send AJAX to R */


    // $('#dataset .dataTables_scrollHeadInner thead th').contextmenu(function(e){
    //     e.preventDefault();
    //     $('#dataset .dataTables_scrollHeadInner thead th').css('color', '#000');
    //     $(e.currentTarget).css('color','var(--color-active)');
    //     console.log($(this).html());
    //     Shiny.onInputChange('key', e.currentTarget.innerHTML);
    // });

    // $('#dataset .dataTables_scrollHeadInner thead th').on('taphold', function(e){
    //     e.preventDefault();
    //     $('#dataset .dataTables_scrollHeadInner thead th').css('color', '#000');
    //     $(e.currentTarget).css('color','var(--color-active)');
    //     console.log($(this).html());
    //     Shiny.onInputChange('key', e.currentTarget.innerHTML);
    // });

    responsiveText();
}

export const _DT_initComplete = function(dt) { 
    // let DT_table = dt;  
    // shadowPlunge('#inputDialog .dialog');
    // $('#DT').attr('dir', 'ltr');
    
    doki_kaboom('#inputDialog .dialog');
    $('#data-table .dataTables_scroll .simplebar-content').scroll(function(){
    let left = $('#data-table .dataTables_scrollBody .simplebar-content').scrollLeft();
    $('#data-table .dataTables_scrollHead').animate({
        scrollLeft: left}, 5);
    });
    // SimpleBar_init('#methodsNav .tab-pane');
    // SimpleBar_init('#dictNav .tab-pane');
    // cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
    // cssVar.contentinnerHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active .simplebar-content .row').height() + 'px';

    const table = dt.api().table();
}

export const nav_init = (el, methods) => {
    methods.map((method, idx) => {
        $($(el).find('ul.nav.nav-pills li a')[idx]).attr('data-code', method);
        $(document).on('click', ` ${el} ul.nav.nav-pills li a`, function(){
            $(`.actionBar[data-for = '${el}'] .btn:not(#all_action)`).addClass('hidden');
            $(`.actionBar .btn#${$(this).data('code')}_action`).removeClass('hidden');
        });
    });

    $(`.actionBar[data-for = '${el}']`).find('.btn:not(#all_action)').addClass('hidden');
    $('.actionBar').find(`.btn#${$(el).find('ul.nav.nav-pills li.active a').data('code')}_action`).removeClass('hidden');
};

export const backBtn = id => $(`<div class = 'backBtn' id = '${id}-backBtn' onclick = "backBtnClickFn('${id}')" ></div>`); 
export const backBtnClickFn = id => {
    logHolder = $(`#${id}`);
    logHolder.removeClass('show', 300)
    .parent().find('.arg-holder div').removeClass('disabled');
    logHolder.parents('.tab-pane').css('overflow-y', 'auto');
} 

export const logHolder_init = el => {
    el
    .height(el.parents('.tab-pane').height())
    .addClass('show', 300)
    .parents('.tab-pane').css('overflow-y','hidden');
    if (!el.hasClass('logHolder-default')) {
        el.addClass('logHolder-default').prepend(backBtn(el.attr('id')));
        SimpleBar_init(`#${el.attr('id')}`);
    }
};

export const ready_state_init = () => {
    $('#grand-top-bar').removeClass('pseudo-hidden');
    $('div.material-switch input').prop('disabled', false);
    $('.navbar-toggle').removeClass('hidden');
    $('.log-holder').removeClass('show');
    $('.arg-holder div').removeClass('disabled');

    $('#header-div').append($('#header'));
    $('#overlay .lds-roller').removeClass('hidden',1000);
    $('#data-input-holder').removeClass('shadowSurge').removeClass('center', 500);
    $($('#data-input-holder label')[0]).addClass('hidden', 500);
    $('#datasource_progress div').html('');

    $('#overlay').fadeOut(1000);
    $('#sidebar-holder').show();

    // if ($('#datasource').val() == ''){
    //     $('#data-input-holder').prepend($('#header'));
    //     $('#sidebar-holder').hide();
    //     $('#data-input-holder').addClass('center', 300);
    //     $($('#data-input-holder label')[0]).removeClass('hidden');
    //     $('#overlay .lds-roller').addClass('hidden',1000);
    // } else {
       
    // }
}

export const isTouchDevice = () => {
    return 'ontouchstart' in document.documentElement;

}

export const shadowPlunge = (obj) => {
    $(obj).removeClass('shadowPurge');
    $(obj).addClass('shadowPlunge');
} 

export const doki_kaboom = (obj) => {
    $(obj).removeClass('shadowPurge');
    $(obj).addClass('doki-kaboom');
} 