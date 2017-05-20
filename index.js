$(document).ready(function() {
	//Make font size of executive summary smaller than a regular paragraph.
	console.log('hello world');
	fontp = $('p').css('font-size');
	fontp = parseInt(fontp);
	fontp = Math.round(0.9 * fontp) + 'px';
	$('.exec-summ').css('font-size', fontp);
	console.log('bye bye');
});
