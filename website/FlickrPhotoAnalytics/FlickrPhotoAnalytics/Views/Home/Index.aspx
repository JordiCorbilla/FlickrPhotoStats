<%@ Page Language="C#" MasterPageFile="~/Views/Shared/Site.Master" Inherits="System.Web.Mvc.ViewPage<FlickrPhotoAnalytics.Models.HomeModel>" %>

<asp:Content ID="Content1" ContentPlaceHolderID="TitleContent" runat="server">
    Home Page
</asp:Content>

<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">
    <h2><%: ViewData["Message"] %></h2>
    <div id="chart1" style="height:300px; width:33%; float: left;"></div>
    <div id="chart2" style="height:300px; width:33%; float: left;"></div>
    <div id="chart3" style="height:300px; width:33%; float: right;"></div>
    <script>
        var $contentOfChart = "<%=Model.values%>";
    </script>
</asp:Content>
