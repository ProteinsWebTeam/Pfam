(function() {
  'use strict';
  var displayMessage = function(message, yes) {
    message.style.display = yes ? 'block' : '';
  };

  var handleFile = function(input, message) {
    input.addEventListener('change', function(e) {
      var file = e.target.files[0];
      if (!file) {
        return displayMessage(message, false);
      }
      file = file.slice(0, 500);
      var reader = new FileReader();
      reader.addEventListener('load', function(e) {
        var fd = new FormData();
        fd.append('seq', e.target.result);
        fetch('/search/sequence/type', {method: 'POST', body: fd})
          .then(function(r) {
            if (r.status >= 400 && r.status < 600) throw new Error();
            return r.text();
          }).then(function(t) {
            displayMessage(message, t !== 'protein');
          }).catch(function() {
            displayMessage(message, true);
          });
      });
      reader.readAsText(file);
    });
  };

  var handleJobStatus = function(container) {
    var input = container.querySelector('input');
    var messages = [].slice.call(container.querySelectorAll('.message'));
    var displayMessage = function(m) {
      messages.forEach(function(message) {
        if (m && message.classList.contains(m)) {
          message.style.display = 'block';
        } else {
          message.style.display = '';
        }
      });
    };
    input.addEventListener('input', function(e) {
      var jobID = e.target.value;
      if (!jobID || !e.target.validity || !e.target.validity.valid) {
        displayMessage();
        return;
      };
      fetch(location.origin + '/search/sequence/resultset/' + jobID)
        .then(function(r) {return r.text()})
        .then(function(m) {
          if (input.value !== jobID) return;
          if (/^(RUN|PEND|HOLD|FAIL)$/i.test(m)) {
            return displayMessage(m.toLowerCase());
          }
          if (m.indexOf('is on hold') !== -1) {
            return displayMessage('hold');
          }
          if (m.indexOf('failed to complete') !== -1) {
            return displayMessage('fail');
          }
          throw new Error();
        })
        .catch(function() {
          if (input.value !== jobID) return;
          displayMessage('problem');
        });
      displayMessage('loading');
    });
  };

  var runAll = function() {
    handleFile(
      document.querySelector('input[type="file"]'),
      document.getElementById('batchCheckFile').firstElementChild
    );
    handleJobStatus(document.getElementById('batchCheck'));
  };

  // Feature support detection
  if (!(window.fetch && window.FormData && window.FileReader)) {
    // Doesn't cut it, don't run anything
    return;
  }
  if (document.readyState !== 'loading') {
    runAll();
  } else {
    document.addEventListener('DOMContentLoaded', runAll);
  }
})();
