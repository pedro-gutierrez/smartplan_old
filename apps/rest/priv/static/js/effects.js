$(document).ready(function () {
    $('body').scrollspy({
        target: '.navbar-fixed-top',
        offset: 80
    });

    // Page scrolling feature
    $('a.page-scroll').bind('click', function(event) {
        var link = $(this);
        var target = $(link.attr('href'));
        if( target && target.length ) {
            $('html, body').stop().animate({
                scrollTop: target.offset().top - 50
            }, 500);
        }
        event.preventDefault();
    });
});

var ar=new Array(33,34,35,36,38,40);

$(document).keydown(function(e) {
  var key = e.which;
  if($.inArray(key,ar) > -1) {
    e.preventDefault();
    return false;
  }
  return true;
});


var cbpAnimatedHeader = (function() {
    var docElem = document.documentElement,
            header = document.querySelector( '.navbar-default' ),
            didScroll = false,
            changeHeaderOn = 200;
    function init() {
        window.addEventListener( 'scroll', function( event ) {
            if( !didScroll ) {
                didScroll = true;
                setTimeout( scrollPage, 250 );
            }
        }, false );
    }
    function scrollPage() {
        var sy = scrollY();
        if ( sy >= changeHeaderOn ) {
            $(header).addClass('navbar-scroll')
        }
        else {
            $(header).removeClass('navbar-scroll')
        }
        didScroll = false;
    }
    function scrollY() {
        return window.pageYOffset || docElem.scrollTop;
    }
    init();

})();

/*

new WOW().init();*/
