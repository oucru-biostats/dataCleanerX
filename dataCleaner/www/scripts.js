/* var declaration */

let cssVar = new CSSGlobalVariables();
let barSize = {barLeft: 0, barRight: 'auto'};
let nav_title = {
    long_form: ['Missing Data','Numerical Outliers','Cartegorical Loners','Binaries','Whitespaces','Spelling Issues', 'Duplicated IDs'],
    short_form: ['MSD','OUTL','LNR','BIN','WSP','SPL','DID']
}
let highlightChkResVal = {};
let chkResVal = {};
let DT_table;

const set_bar = (x, y) => {
    barSize.barLeft = x;
    barSize.barRight = y;
    cssVar.barLeft = barSize.barLeft + 'px';
    cssVar.barRight = `calc(100% - ${barSize.barRight}px)`;
};

const set_inputDT = function(table){
    table.cells().every(function(i, j, tab, cell) {
        var $this = $(this.node());
        if ($this.find('input').length > 0){
          $this.find('input').addClass('form-control');
        }
      });
}

const tagParse = function(table){
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

const SimpleBar_init = (elList) => {
    $(document).find(elList).each(function(idx, el) {
        new SimpleBar(el);
        $(el).css('overflow','unset');
    });
}

const responsiveText = function(){
    cssVar.grandTabTop = ($('#dataset').position().top + $('#dataset').outerHeight()) + 'px';
    if ($(window).width() < 768){
        $('.paginate_button.previous').html('◄');
        $('.paginate_button.next').html('►');
    } else {
        $('.paginate_button.previous').html('Previous');
        $('.paginate_button.next').html('Next');
    }    
}

const _init_methodsSection = () => {
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

const _DT_highlight = (dt, rows, col) => {    
    rows.map(row => {
        $(dt.row(row).node()).addClass('highlight', 300);
        $(dt.cell(row, col).node()).addClass('highlight', 300);
    })
}

async function get_highlightCol(dt, highlightVar){
    headers = $(dt.header()).find('th').map(function() {return this.innerText});
    highlightCol = [];
    headers.map((idx, header) => {
        if (highlightVar.includes(header)) highlightCol.push(idx);
    });
    
    return highlightCol;
}

async function get_highlightRow(dt, highlightIdx){
    idCol = dt.column(0).data();
    highlightRow = [];
    idCol.map((id, idx) => {
        if (highlightIdx.includes(Number(id))) highlightRow.push(idx);
    });

    return highlightRow;
}

async function get_highlightCoord(dt, highlightChkResVal){
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

const _DT_callback = function(dt) {
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

const _DT_initComplete = function(dt) { 
    DT_table = dt;  
    $('#dataset .dataTables_scroll .simplebar-content').scroll(function(){
    let left = $('#dataset .dataTables_scrollBody .simplebar-content').scrollLeft();
    $('#dataset .dataTables_scrollHead').animate({
        scrollLeft: left}, 5);
    });
    SimpleBar_init('#methodsNav .tab-pane');
    SimpleBar_init('#dictNav .tab-pane');
    cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
    cssVar.contentinnerHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active .simplebar-content .row').height() + 'px';

    const table = dt.api().table();

    ready_state_init();
    
    tippy('.res-log td', {
        theme: 'light-border',
        size: 'large',
        touch: true,
        placement: 'top-start',
        duration: 500,
        interactive: true,
        trigger: 'click'
    });

    _init_methodsSection();
}

const nav_init = (el, methods) => {
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

const backBtn = id => $(`<div class = 'backBtn' id = '${id}-backBtn' onclick = "backBtnClickFn('${id}')" ></div>`); 
const backBtnClickFn = id => {
    logHolder = $(`#${id}`);
    logHolder.removeClass('show', 300)
    .parent().find('.arg-holder div').removeClass('disabled');
    logHolder.parents('.tab-pane').css('overflow-y', 'auto');
} 

const logHolder_init = el => {
    el
    .height(el.parents('.tab-pane').height())
    .addClass('show', 300)
    .parents('.tab-pane').css('overflow-y','hidden');
    if (!el.hasClass('logHolder-default')) {
        el.addClass('logHolder-default').prepend(backBtn(el.attr('id')));
        SimpleBar_init(`#${el.attr('id')}`);
    }
};

const ready_state_init = () => {
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

const isTouchDevice = () => {
    return 'ontouchstart' in document.documentElement;

}


/* jquery code run */

$(document).ready(() => {
    cssVar.datasetTop = $(window).width() > 500 ? '70px' : '120px';
    $('#grand-top-bar').addClass('pseudo-hidden');
    $('.navbar-toggle').addClass('hidden');
    set_bar($('#grand-top-bar li.active').position().left, $('#grand-top-bar li.active').position().left + $('#grand-top-bar li.active').outerWidth());
    $('#data-input-holder').prepend($('#header')).addClass('shadowSurge');
    // $(document).on('click', '#grand-top-bar li a', () => $('.actionBar').find(`.btn#${$('.navMenu').find('ul.nav.nav-pills li.active a').data('code')}_action`).removeClass('hidden'));

    $('#grand-top-bar').find('li').hover(function(){
        if ($(this).position().left > barSize.barLeft)
            set_bar(barSize.barLeft, $(this).position().left + $(this).outerWidth());
        else
            set_bar($(this).position().left, barSize.barRight);
        setTimeout(set_bar, 150, $(this).position().left, $(this).position().left + $(this).outerWidth());
    }, function(){
            setTimeout(set_bar, 150, $('#grand-top-bar li.active').position().left, $(`#grand-top-bar li.active`).position().left + $('#grand-top-bar li.active').outerWidth());
    }).click(function(){
        set_bar($(this).position().left, $(this).position().left + $(this).outerWidth());
    });

    $('#grand-top-bar').find('li a').on('click', function(){
        set_bar($(this).parent().position().left, $(this).parent().position().left + $(this).parent().outerWidth());
    });

    $("#datasource").on("change", function() {
        $('#overlay').fadeIn(1000);
    });

    $('#data-input input:text').click(function(){
        $($(this).parent()).find('label.input-group-btn').click();
    });
});

/* swipe event */

$('nav').on('swipeleft', (event) => {
    idx = 0;
	document.querySelectorAll('#grand-top-bar li').forEach(function(item, index){
    	if ($(item).hasClass('active')) {
			idx = index;
         }
	});

    if(document.querySelectorAll('#grand-top-bar li a')[idx + 1]) 
        $($('#grand-top-bar').find('li a')[idx + 1]).click(); 
    else $($('#grand-top-bar').find('li a')[0]).click();
});

$('nav').on('swiperight', () => {
    idx = 0;
	document.querySelectorAll('#grand-top-bar li').forEach(function(item, index){
    	if ($(item).hasClass('active')) {
			idx = index;
         }
	});

    if(document.querySelectorAll('#grand-top-bar li a')[idx - 1]) 
        $($('#grand-top-bar').find('li a')[idx - 1]).click(); 
    else $($('#grand-top-bar').find('li a')[$('#grand-top-bar').find('li a').length - 1]).click();
});

$(document).on('click', '.actionBar button', function(){
    $(this).addClass('working');
})

/* resize event */
$(window).resize(() => {
    set_bar($('#grand-top-bar li.active').position().left, $('#grand-top-bar li.active').position().left + $('#grand-top-bar li.active').outerWidth());
    cssVar.datasetTop = $(window).width() > 500 ? '70px' : '120px';
    cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
    cssVar.contentinnerHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active .simplebar-content .row').height() + 'px';
    responsiveText();  
})

/* R listener */
Shiny.addCustomMessageHandler("excel", function(isTRUE){
    if(isTRUE) {
        $('#sheet-input').removeClass('pseudo-hidden',300);
        $("#data-input-holder #data-input").removeClass('fullWidth', 300);
    } else {
        $('#sheet-input').addClass('pseudo-hidden',300);
        $("#data-input-holder #data-input").addClass('fullWidth', 300);
    }
});

Shiny.addCustomMessageHandler('changeSheet', function(empty){
    $('#overlay').fadeIn(1000);
    $('#overlay .lds-roller').removeClass('hidden', 1000);
});

Shiny.addCustomMessageHandler('outl_model', function(model){
    if (model === 'adjusted' | model === 'boxplot'){
        $('#outl_fnLower').prop('disabled', true)
        $('#outl_fnUpper').prop('disabled', true)
    } else {
        $('#outl_fnLower').prop('disabled', false)
        $('#outl_fnUpper').prop('disabled', false)
    }
});

Shiny.addCustomMessageHandler('has_keys', function(has_keys){
    $('#did_repNo').prop('disabled', has_keys)
});

Shiny.addCustomMessageHandler('disabledMethod', disabled => disabled.map(value => $(`#${value}`).prop('disabled', true)));

Shiny.addCustomMessageHandler('logOn', id => {
    let logHolder = $(`#${id}-log-holder`);
    let argHolder = $(`#${id}_options`);
    // logHolder.height(Math.max(logHolder.parent().height() - 5, $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() -5)).addClass('show', 300);
    argHolder.addClass('disabled');
    $(`#${id}_action`).removeClass('working');
    $('#all_action').removeClass('working');

    logHolder_init(logHolder);
    cssVar.contentinnerHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active .simplebar-content .row').height() + 'px';

});

Shiny.addCustomMessageHandler('res', res => {
    chkResVal[res.test] = null;
    chkResVal[res.test] = res.val;
    if ($(`#methodsNav ul.nav.nav-pills li.active a`).data('code') == res.test) {
        highlightChkResVal = res.val;
        _DT_callback(DT_table);
    }
});

Shiny.addCustomMessageHandler('haveData', haveData => {
    if (haveData) $('#overlay').fadeOut(1000);
});

Shiny.addCustomMessageHandler('all_check', aliases => Object.keys(aliases).map(key => {
    if (aliases[key]) $(`#actionCheckBar div.each-action-holder button#${key}_action`).click()
}));

//<div>Icons made by <a href="https://www.flaticon.com/authors/google" title="Google">Google</a> from <a href="https://www.flaticon.com/" 			    title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" 			    title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
//<div>Icons made by <a href="https://www.flaticon.com/authors/lucy-g" title="Lucy G">Lucy G</a> from <a href="https://www.flaticon.com/" 			    title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" 			    title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>