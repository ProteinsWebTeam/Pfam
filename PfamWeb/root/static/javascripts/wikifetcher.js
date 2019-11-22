// https://en.wikipedia.org/w/api.php?action=parse&format=json&prop=text&page=piwi


const URL = "http://en.wikipedia.org/w/api.php?action=parse&format=json&prop=text&page=";


document.addEventListener('DOMContentLoaded', (event) => {
  const wikiSpans = document.getElementsByClassName("wikipedia");
  for(let i = 0; i < wikiSpans.length; i++ ) {
      const wikiSpan = wikiSpans.item(i);
      const wikiTitle = wikiSpan.getAttribute('data-wikipedia-title');
      const wikiURL = `${URL}${wikiTitle}`;
      const titlePromise = fetch(wikiURL, {mode: 'cors'});
      titlePromise.then(response => {
        if (response.ok) {
          return response.json();
        } else {
            throw `Wikipedia fetch received status=${response.status}`;
        }
      }).then(data => {
          if (wikiSpan) {
            wikiSpan.innerHTML = data.parse.text["*"];
          }
      }).catch(err => {
        console.log(`Error fetching data from wikipedia: ${err}`);
        if (wikiSpan) {
          wikiSpan.innerHTML = `
            <p />Something went wrong while fetching data from Wikipedia. This may
            be a temporary problem so please attempt to reload then page.
            <p />If you continue to see this message please let us know by sending
            an email to <a href="mailto:pfam-help@ebi.ac.uk">pfam-help@ebi.ac.uk</a>
            . Please remember to let us know which Browser you're using.
          `;
        }
        const pfamTab = document.getElementById('pfam_tab');
        if (pfamTab) {
          pfamTab.click();
        }
      });
  }

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
