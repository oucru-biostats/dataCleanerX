let navbar = $(document).find('#fullProgram .navbar');
navbar.find('.container-fluid .navbar-header').append($('#inputBox'));
// navbar.find('.navbar-brand').prepend('🤖 ');
navbar.append('<div id="tab-underline"></div>');

const navbarNav = $('#fullProgram .navbar .nav.navbar-nav');
let activeTab = navbarNav.find('li.active');


const set_bar = (x, y) => {
    barSize.barLeft = x;
    barSize.barRight = y;
    cssVar.barLeft = barSize.barLeft + 'px';
    cssVar.barRight = `calc(100% - ${barSize.barRight}px)`;
};

//change on changetabbar;
navbarNav.find('li').click(function(){
    left = $(this).position().left;
    right = $(this).position().left + $(this).outerWidth();

    if (left != barSize.barLeft || right != barSize.barRight) {
        if (left < barSize.barLeft) {
            set_bar(left, barSize.barRight);
        }

        if (right > barSize.barRight){
            set_bar(barSize.barLeft, right);
        }

        setTimeout(set_bar, 150, left, right);
    } 
});

set_bar(activeTab.position().left,  activeTab.position().left + activeTab.outerWidth());

