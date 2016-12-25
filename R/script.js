$(document).ready(function(){
  setTimeout(function() {
    $(".leaflet-right").remove();
  }, 1000);
  
  $(window).load(function(){
    setTimeout(function() {
      if (typeof(Storage) !== "undefined") {
        if (!localStorage.getItem("help")) {
          $('#myModal').modal('show');
          localStorage.setItem("help", true);
        }
      }
    }, 3000);
  });
});


/* Set the transform translateX to 0 to collapse the sidebar */
function openSidebar() {
  sidebar = $("#collapse_sidebar");
  sidebar.scrollTop(0);
  sidebar.hasClass("collapsed-sidebar") ?
    sidebar.removeClass("collapsed-sidebar") :
    sidebar.addClass("collapsed-sidebar");
}