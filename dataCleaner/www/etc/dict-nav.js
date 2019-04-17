responsiveDictNav = () => {
    $('#dictNav ul').removeClass('collapsed');
    $('#dictNav ul').removeClass('collapsed');
    $('#dictNav ul li a').off('click');
    $('#dictNav').off('swiperight');
    cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
    $('#dictNav ul.nav.nav-pills li').click(function(){
        cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
        cssVar.contentinnerHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active .simplebar-content .row').height() + 'px';
    });


    $('.each-action-holder').removeClass('hidden');
    if ($(window).width() <= 500){
        
        $('#dictNav ul li a').click(function(){
            $('#dictNav ul').toggleClass('collapsed');
            $('.each-action-holder').toggleClass('hidden');
        });
        
        $('#dictNav').on('swiperight', function(){
            $('#dictNav ul').removeClass('collapsed');
            $('.each-action-holder').addClass('hidden');
        });
        $('.each-action-holder').addClass('hidden');
    } else if ($(window).width() < 768){
        $('#dictNav ul li a').click(function(){
            $('#dictNav ul').toggleClass('collapsed');
        });

        $('#dictNav .col-sm-9').mousedown(() => {
            if (!$('#dictNav ul').hasClass('collapsed')){
                cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
                $('#dictNav ul').addClass('collapsed');
            }
        });
        
        $('#dictNav').on('swiperight', function(){
            cssVar.contentHeight = $('.tab-pane.grand-tab-panel.active .tab-pane.active').height() + 'px';
            $('#dictNav ul').removeClass('collapsed');
        });
        $('.each-action-holder').removeClass('hidden');
    } 
}

responsiveDictNav();

$(window).resize(() => {
    responsiveDictNav();
});

nav_init('#dictNav', ['dictCheck', 'dictCreate']);
SimpleBar_init('#dictNav .tab-pane');
