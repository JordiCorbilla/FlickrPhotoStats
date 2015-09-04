$(document).ready(function () {
    var line1 = [['2008-08-12 4:00PM', 4], ['2008-09-12 4:00PM', 6.5], ['2008-10-12 4:00PM', 5.7], ['2008-11-12 4:00PM', 9], ['2008-12-12 4:00PM', 8.2]];
    var plot1 = $.jqplot('chart1', [line1], {
        title: 'Default Date Axis',
        axes: {
            xaxis: {
                renderer: $.jqplot.DateAxisRenderer
            }
        },
        series: [{ lineWidth: 4, markerOptions: { style: 'square'}}]
    });
});