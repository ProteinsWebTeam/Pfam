!function ($) {

  $(function(){
    $('.carousel').carousel({delay:2000});

    $('h2.pfam').bind('mouseover', function() {
      $('.carousel').carousel(0);
    });
    $('h2.rfam').bind('mouseover', function() {
      $('.carousel').carousel(1);
    });
    $('h2.dfam').bind('mouseover', function() {
      $('.carousel').carousel(2);
    });
    $('h2.treefam').bind('mouseover', function() {
      $('.carousel').carousel(3);
    });
    $('h2.ipfam').bind('mouseover', function() {
      $('.carousel').carousel(4);
    });

  })

}(window.jQuery)
