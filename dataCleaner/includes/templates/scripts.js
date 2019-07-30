$(document).ready(function(){

    $(document).find('.section.level1').each(function(){
        $(this).find('h1')
        .prepend(`<button id="${$(this).attr('id')}-btn" for="${$(this).attr('id')}" class="btn btn-primary btn-section"><span class="caret"></span></button>`)
        .click(function(){
            $(this).parent().toggleClass('show', 300);
        });
    });
});