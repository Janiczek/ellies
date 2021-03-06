javascript:(function() {
  if (document.location.origin !== 'https://ellie-app.com') {
    alert('Try this bookmarklet on an Ellie page!');
    return;
  }
  var id = document.location.pathname.slice(1);
  if (id === 'new') {
    alert("Didn't save to the catalog yet - you'll have to save your Ellie first!");
    return;
  }
  var name = prompt('Name the Ellie');
  if (name == null) {
    return;
  }
  var xhr = new XMLHttpRequest();
  xhr.onload = function() {
    if (xhr.status >= 200 && xhr.status < 300) {
      alert(xhr.response);
    } else {
      alert('Something went wrong :( Ping @janiczek on Twitter or Elm Slack!');
    }
  };
  xhr.open('GET','https://janiczek-ellies.builtwithdark.com/save?id=' + encodeURIComponent(id) + '&name=' + encodeURIComponent(name));
  xhr.send();
}())
