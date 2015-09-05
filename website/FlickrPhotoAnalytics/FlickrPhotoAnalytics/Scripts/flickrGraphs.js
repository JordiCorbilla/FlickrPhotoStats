//****************************************************************************
// jQuery Flickr Graph
// @Author: J Corbilla.
// @Description: This unit uses jqplot to display the content of the xml file
// 2015
//*****************************************************************************

$(document).ready(function () {
    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchData?id=1",  
        type: 'GET',  
        success: function (result) {  
            AddPlot('chart1', result, 'Number of Views');  
        },  
        error: function () {  
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchData?id=2",
        type: 'GET',
        success: function (result) {
            AddPlot('chart2', result, 'Number of Likes');
        },
        error: function () {
        }
    });

    $.ajax({
        url: "/FlickrPhotoAnalytics/home/fetchData?id=3",
        type: 'GET',
        success: function (result) {
            AddPlot('chart3', result, 'Number of Comments');
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
        var plot2 = $.jqplot(chart, [line1], {  
            cursor: {
                    show: true,
                    zoom: true
                },
                title: gtitle,  
            axes: {  
                xaxis: {  
                    renderer: $.jqplot.DateAxisRenderer,  
                    tickOptions: { formatString: '%b %#d, %y' },  
                    pad: 0  
                },  
                yaxis: {  
                }  
                },  
            series: [{ lineWidth: 2  
                ,markerOptions: { style: 'filledCircle', size: 0}  
                }]  
        });  
    }
});  
