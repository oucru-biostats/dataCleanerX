responsiveMethodsNav = () => {
    $('#methodsNav ul.nav.nav-pills').removeClass('collapsed');
    $('#methodsToggle').removeClass('collapsed');
    $('.each-action-holder').removeClass('hidden');
    $('#methodsNav ul.nav.nav-pills').removeClass('collapsed');
    $('#methodsNav ul.nav.nav-pills li a').off('click');
    $('#methodsNav').off('swiperight');

    cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';

    $('#methodsNav ul.nav.nav-pills li').click(function(){
        cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
        cssVar.contentinnerHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active .simplebar-content .row').height() + 'px';
        code = $(this).find('a').data('code');
        if (chkResVal[code]) highlightChkResVal = chkResVal[code];
    });

    if ($(window).width() <= 500){
        $('#methodsNav ul.nav.nav-pills li a').click(function(){
            $('#methodsNav ul.nav.nav-pills').toggleClass('collapsed');
            $('#methodsToggle').toggleClass('collapsed');
            $('.each-action-holder').toggleClass('hidden');
            cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
        });
        
        $('#methodsNav').on('swiperight', function(){
            $('#methodsNav ul.nav.nav-pills').removeClass('collapsed');
            $('#methodsToggle').removeClass('collapsed');
            $('.each-action-holder').addClass('hidden');
        });

        $('.each-action-holder').addClass('hidden');
    } else if ($(window).width() < 768){
        $('#methodsNav ul.nav.nav-pills li a').click(function(){
            $('#methodsNav ul.nav.nav-pills').toggleClass('collapsed');
            $('#methodsToggle').toggleClass('collapsed');
            cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
        });

        $('#methodsNav .col-sm-9').mousedown(() => {
            if (!$('#methodsNav ul.nav.nav-pills').hasClass('collapsed')){
                $('#methodsNav ul.nav.nav-pills').addClass('collapsed');
                $('#methodsToggle').addClass('collapsed');
            }
        });
        
        $('#methodsNav').on('swiperight', function(){
            $('#methodsNav ul.nav.nav-pills').removeClass('collapsed');
            $('#methodsToggle').removeClass('collapsed');
            cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
        });
        $('.each-action-holder').removeClass('hidden');
    } else {
        $('#methodsNav .col-sm-9').dblclick(() => {
            $('#methodsNav ul.nav.nav-pills').addClass('collapsed');
            $('#methodsToggle').addClass('collapsed');
        });
    }
};


$('#methodsNav div.col-sm-4.well')
.removeClass('col-sm-4')
.addClass('col-sm-3')
.parent().find('div.col-sm-8').removeClass('col-sm-8').addClass('col-sm-9');

responsiveMethodsNav();
$(window).resize(() => {
    responsiveMethodsNav();
});

nav_init('#methodsNav', ['msd', 'outl', 'lnr', 'bin', 'wsp', 'spl', 'did']);

$(document).on('dblclick', '.log-holder .chk-logTable td', function() {
    $(this).toggleClass('showAll', 300)
    .next().toggleClass('showAll', 300);
});




