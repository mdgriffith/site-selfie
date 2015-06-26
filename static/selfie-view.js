$( document ).ready(function() {
  $(".comparison").click(function(){
    var $this = $(this);
    if ($this.hasClass("being-viewed")){
      $('.being-viewed').removeClass("being-viewed");
      $(".backdrop").removeClass("is-viewing");
    } else {
      $('.being-viewed').removeClass("being-viewed");
      $this.addClass("being-viewed");
      $(".backdrop").addClass("is-viewing");
    }
    


  });
  $(".backdrop").click(function(){
    if ($(this).hasClass("is-viewing")){
      $('.being-viewed').removeClass("being-viewed");
      $(".backdrop").removeClass("is-viewing");
    }
  });
});