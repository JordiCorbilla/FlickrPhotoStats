//****************************************************************************
// flickrGraphs.js
// jQuery Flickr Graph
// @Author: J Corbilla.
// @Description: This unit uses jqplot to display the content of the xml file
// 2015
//*****************************************************************************

$(document).ready(function () {
    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchTotals?view=views",  
        type: 'GET',  
        success: function (result) {  
            AddPlot('chart1', result, 'Number of Views');  
        },  
        error: function () {  
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchTotals?view=likes",
        type: 'GET',
        success: function (result) {
            AddPlot('chart2', result, 'Number of Likes');
        },
        error: function () {
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchTotals?view=comments",
        type: 'GET',
        success: function (result) {
            AddPlot('chart3', result, 'Number of Comments');
        },
        error: function () {
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchPartials?view=views",
        type: 'GET',
        success: function (result) {
            AddPlot('chart4', result, 'Number of Views');
        },
        error: function () {
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchPartials?view=likes",
        type: 'GET',
        success: function (result) {
            AddPlot('chart5', result, 'Number of Likes');
        },
        error: function () {
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchPartials?view=comments",
        type: 'GET',
        success: function (result) {
            AddPlot('chart6', result, 'Number of Comments');
        },
        error: function () {
        }
    });

    function AddPlot(chart, result, gtitle) {
        var arr = [];
        for (var i = 0; i < result.stats.length; i++) {
            var date = result.stats[i].Date;
            var value = result.stats[i].Value;
            arr.push([new Date(date), value]);
        }

        var line1 = arr;
        $.jqplot.config.enablePlugins = true;
        $.jqplot(chart, [line1], {
            seriesColors: ["rgba(78, 135, 194, 0.7)"],
            title: gtitle,
            grid: {
                background: 'rgba(57,57,57,0.0)',
                drawBorder: false,
                shadow: false,
                gridLineColor: '#666666',
                gridLineWidth: 2
            },
            seriesDefaults: {
                rendererOptions: {
                    smooth: true,
                    animation: {
                        show: true
                    }
                },
                showMarker: false
            },
            axesDefaults: {
                rendererOptions: {
                    baselineWidth: 1.5,
                    baselineColor: '#444444',
                    drawBaseline: false
                }
            },
            cursor: {
                    show: true,
                    zoom: true
            },
            axes: {  
                xaxis: {
                    renderer: $.jqplot.DateAxisRenderer,
                    tickRenderer: $.jqplot.CanvasAxisTickRenderer,
                    tickOptions: {
                        formatString: '%#d/%#m/%y',
                        angle: -30,
                        textColor: '#dddddd'
                        },  
                    pad: 0  
                },
                yaxis: {
                    renderer: $.jqplot.LogAxisRenderer,
                    min: 0
                }
            },
            highlighter: {
                sizeAdjust: 10,
                tooltipLocation: 'n',
                tooltipAxes: 'y',
                tooltipFormatString: '<b><i><span style="color:red;">Value</span></i></b> %.2f',
                useAxesFormatters: false
            },
            series: [{ lineWidth: 2
                , markerOptions: { style: 'filledCircle', size: 1 },  
                trendline: {
                    color: 'rgb(211, 235, 59)'
                }
                }]
        });
    }
});  
