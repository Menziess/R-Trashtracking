$(document).ready(function(){
  setTimeout(function() {
    $(".leaflet-right").remove();
  }, 1000);

  $('[data-toggle="popover"]').popover(); 

});

/* Set the transform translateX to 0 to collapse the sidebar */
function openSidebar() {
  sidebar = $("#collapse_sidebar");
  sidebar.hasClass("collapsed-sidebar") ?
    sidebar.removeClass("collapsed-sidebar") :
    sidebar.addClass("collapsed-sidebar");
}