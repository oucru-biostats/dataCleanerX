(async () => {
let {set_bar, change_bar} = await import('../etc/css-var.js');
let navbar = $(document).find('#fullProgram .navbar');
navbar.append('<div id="tab-underline"></div>').find('.container-fluid .navbar-header').append($('#inputBox'));

const navbarNav = $('#fullProgram .navbar .nav.navbar-nav');
let activeTab = navbarNav.find('li.active');

//change on changetabbar;
navbarNav.find('li').click(function(){change_bar(this)});

set_bar(activeTab.offset().left,  activeTab.offset().left + activeTab.outerWidth());

$(window).resize(() => set_bar(navbarNav.find('li.active').offset().left,  navbarNav.find('li.active').offset().left + navbarNav.find('li.active').outerWidth()));

})();