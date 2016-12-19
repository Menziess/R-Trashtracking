$(document).ready(function(){
  setTimeout(function() {
    $(".leaflet-right").remove();
  }, 1000);
});

/* Set the transform translateX to 0 to collapse the sidebar */
function openSidebar() {
  sidebar = document.getElementById("collapse_sidebar");
  if (sidebar.style.transform == "translateX(0em)") {
      sidebar.style.transform = "translateX(30em)";
  } else {
      sidebar.style.transform = "translateX(0em)";
  }
}