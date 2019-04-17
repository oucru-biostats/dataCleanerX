
$("#dataset-menu").menu({
    position: { my: "right top", at: "left+5 top+5" }
});
$("#output-holder").addClass("hidden"); 
$('#shownColumns').addClass('awesome-checkbox-menu');
$('#keyVariable').addClass('awesome-checkbox-menu');
SimpleBar_init("#shownColumns");
$('#dataOptions .awesome-checkbox').click(function(e){
    if (!$(this).find('input').is(e.target) && !$(this).find('label').is(e.target)) $(this).find('input').click();
});

$('#dataOptions .radio-bs').click(function(e){
    if (!$(this).find('input').is(e.target) && !$(this).find('label').is(e.target)) $(this).find('input').click();
});

let last_default = null;
$('#dataOptions .awesome-checkbox').contextmenu(function(e){
    e.preventDefault();
    console.log($(this).find('label').html());
    Shiny.onInputChange('idCol', $(this).find('label').html());
    $(this).find('label').toggleClass('defaultID');
    $(last_default).toggleClass('defaultID');
    last_default = $(this).find('label');
    // if (!$(this).find('input').is(e.target) && !$(this).find('label').is(e.target)) $(this).find('input').click();
});

$('#columns-chooser-holder').mouseup(function(){
    $('#show-hide-columns').trigger('mouseover');
    $('#show-hide-columns').trigger('click');
})

tippy(document.querySelectorAll('#dataOptions .awesome-checkbox'),{
    content: "<span style='font-size: 12.5px'>Right click to set ID column.</span>",
    theme: "light-border",
    size: 'large',
    touch: true,
    placement: 'top-start',
    interactive: true,
    duration: 500}
);

$('#dataOptions .awesome-checbox').on('click','.tippy-content', function(e){
    $(this).trigger('contextmenu');
});

$('#dataOptions #save-settings-btn').click(function(){
    $(this).parent().find('#saveSettings').click();
});

$('#dataOptions #load-settings-btn').click(function(){
    $(this).parent().find('label.input-group-btn').click();
})

/* R listener for dataoptions */

Shiny.addCustomMessageHandler('toggleCol', function(changeList){
    changeList.forEach(function(col){
        table = $('#dataset').find('table').DataTable();
        table.column(col).visible() === true ? table.column(col).visible(false) : table.column(col).visible(true);
    });
});