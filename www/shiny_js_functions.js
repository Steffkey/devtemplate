//display tooltips
$(document).on('shiny:bound', function(event) {
  console.log('Shiny bound: initializing tooltips');
  $('.info_icon').tooltip({
	placement: 'left',
	trigger: 'hover',
	delay: { 'show': 0, 'hide': 3000 },
	title: function() {
	  return $(this).attr('data-tooltip');
	}
  });
});

//land on top of the page after clicking previous/next button
Shiny.addCustomMessageHandler('scrollTop', function(message) {
   window.scrollTo(0, 0);
});

//open link in new window when browsing for examples
$(document).on("shiny:connected", function(e) {
	  Shiny.addCustomMessageHandler("openNewWindow", function(message) {
		window.open(message.url, "_blank");
	  });
});	

//resize textarea depending on input	
$(document).on("input", "textarea", function() {
	$(this).height(0);
	$(this).height(this.scrollHeight);
});