// https://en.wikipedia.org/w/api.php?action=parse&format=json&prop=text&page=piwi


const URL = "http://en.wikipedia.org/w/api.php?action=parse&format=json&prop=text&page=[% title.get_column('title') %]"


document.addEventListener('DOMContentLoaded', (event) => {
  console.log("loaded");
  const titleTags = document.getElementsByClassName("wiki_title");
  console.log(`TITLES: ${titleTags}`);
  /*
  fetch(URL, {mode: 'cors'}).then(function(response) {
    console.log("STATUS: " + response.status);
    console.log("STATUS: " + response.type);
    console.log("STATUS: " + response.ok);
    if (response.ok) {
      return response.json();
    } else {
      const pfamTab = document.getElementById("wikipedia");
      throw "not ok";
    }
  }).then(function(data) {
    console.log(data);
    const element = document.getElementById("wikipedia");
    element.innerHTML = data.parse.text;
  }).catch (function(err) {
    console.log(`Failed to fetch data from wikipedia: ${err}`);
  })
  */
});
