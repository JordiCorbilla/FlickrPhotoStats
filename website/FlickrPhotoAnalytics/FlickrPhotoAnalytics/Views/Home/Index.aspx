<%@ Page Language="C#" MasterPageFile="~/Views/Shared/Site.Master" Inherits="System.Web.Mvc.ViewPage<FlickrPhotoAnalytics.Models.HomeModel>" %>

<asp:Content ID="Content1" ContentPlaceHolderID="TitleContent" runat="server">
    Home Page
</asp:Content>

<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">
    <h2><%: ViewData["Message"] %></h2>
    <p>
        Total Views.
    </p>
    <div id="chart1" style="height:300px; width:950px;"></div>
    <p>
        Total Likes.
    </p>
    <div id="chart2" style="height:300px; width:950px;"></div>
    <p>
        Total Comments.
    </p>
    <div id="chart3" style="height:300px; width:950px;"></div>
    <script>
        var $contentOfChart = "<%=Model.values%>";
    </script>
</asp:Content>
