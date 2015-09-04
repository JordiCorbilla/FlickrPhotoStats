$(document).ready(function () {
    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchData?id=1",  
        type: 'GET',  
        success: function (result) {  
            AddPlot('chart1', result.stats, result.days);  
        },  
        error: function () {  
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchData?id=2",
        type: 'GET',
        success: function (result) {
            AddPlot('chart2', result.stats, result.days);
        },
        error: function () {
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchData?id=3",
        type: 'GET',
        success: function (result) {
            AddPlot('chart3', result.stats, result.days);
        },
        error: function () {
        }
    });

    function AddPlot(chart, stats, days) {
        $.jqplot(chart, [stats], {
            axes: {
                xaxis: {
                    //ticks: days,  
                    renderer: $.jqplot.DateAxisRenderer
//                    tickRenderer: $.jqplot.CanvasAxisTickRenderer,
//                    tickInterval: '6 months',
//                    tickOptions: {
//                        angle: -30
//                    }
                }
//                ,
//                yaxis: {
//                    renderer: $.jqplot.LogAxisRenderer,
//                    tickOptions: { prefix: '$' }
//                } 
        },
                cursor: {
                    show: true,
                    zoom: true
                } 
            ,
            series: [{
                lineWidth: 1, shadow: false,
                rendererOptions: { smooth: false },
                markerOptions: { show: true, shadow: false, size: 2 } 
//                renderer: $.jqplot.OHLCRenderer,
//                lineWidth: 2
//                , markerOptions: { style: 'square' }
                }]
        });
    } 
});  
