$(document).ready(function(){
  setTimeout(function() {
    $(".leaflet-right").remove();
  }, 1000);
});

/* Set the width of the side navigation to 35em */
function openSidebar() {
  sidebar = document.getElementById("collapse_sidebar");
  content = document.getElementById("sidebar_content");
  if (sidebar.style.width == "35em") {
    sidebar.style.width = "5em";
    content.style.display = "none";
  } else {
    sidebar.style.width = "35em";
    content.style.display = "";
  }
}