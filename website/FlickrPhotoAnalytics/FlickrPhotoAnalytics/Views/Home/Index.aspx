<%@ Page Language="C#" MasterPageFile="~/Views/Shared/Site.Master" Inherits="System.Web.Mvc.ViewPage<FlickrPhotoAnalytics.Models.HomeModel>" %>

<asp:Content ID="Content1" ContentPlaceHolderID="TitleContent" runat="server">
    Home Page
</asp:Content>

<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">
    <h2><%: ViewData["Message"] %></h2>
    <style type="text/css">
        .jqplot-target {
            height: 340px;
            width: 600px;
            color: #dddddd;
        }

        table.jqplot-table-legend {
            border: 0px;
            background-color: rgba(100,100,100, 0.0);
        }

        .jqplot-highlighter-tooltip {
            background-color: rgba(57,57,57, 0.9);
            padding: 7px;
            color: #dddddd;
        }
    </style>
    <div class="ui-wrapper-content">
        <div id="chart1" style="height:300px; width:33%; float: left; background: rgb(57,57,57); margin-right: 9px;"></div>
        <div id="chart2" style="height:300px; width:33%; float: left; background: rgb(57,57,57); "></div>
        <div id="chart3" style="height:300px; width:33%; float: right; background: rgb(57,57,57); "></div>
    </div>
    <script>
        var $contentOfChart = "<%=Model.values%>";
    </script>
</asp:Content>
