using System;
using System.Collections.Generic;
using System.Web.Mvc;
using System.Xml;
using FlickrPhotoAnalytics.lib;
using FlickrPhotoAnalytics.Models;

namespace FlickrPhotoAnalytics.Controllers
{
    [HandleError]
    public class HomeController : Controller
    {
        public ActionResult Index(HomeModel model)
        {
            ViewData["Message"] = "Dashboard";
            model.values = "[['2008-08-12 4:00PM',4], ['2008-09-12 4:00PM',6.5], ['2008-10-12 4:00PM',5.7], ['2008-11-12 4:00PM',9], ['2008-12-12 4:00PM',8.2]]";
            return View(model);
        }

        public ActionResult About()
        {
            return View();
        }

        public class SingleStats
        {
            public String Date;
            public Nullable<int> Value;
        }

        public ActionResult fetchTotals(string view)
        {
            XmlDocument doc = new XmlDocument();
            doc.Load("c:\\temp\\flickrRepositoryGlobal.xml");
            var data = new List<IStat>();

            // cycle through each child noed
            foreach (XmlNode node in doc.DocumentElement.ChildNodes)
            {
                IStat s = new Stat();
                DateTime p;
                DateTime.TryParse(node.Attributes["Date"].Value, out p);
                s.date = p;
                s.views = Int32.Parse(node.Attributes["Views"].Value);
                s.comments = Int32.Parse(node.Attributes["Comments"].Value);
                s.likes = Int32.Parse(node.Attributes["Likes"].Value);
                data.Add(s);
            }

            var list = new List<SingleStats>();

            foreach (var item in data)
            {
                SingleStats stat = new SingleStats() { Date = item.date.ToString("d-MMM-yyyy") };

                if (view == "views")
                    stat.Value = item.views;
                else if (view == "likes")
                    stat.Value = item.likes;
                else if (view == "comments")
                    stat.Value = item.comments;
                list.Add(stat);
            }

            return Json(new { stats = list }, JsonRequestBehavior.AllowGet);
        }

        public ActionResult fetchPartials(string view)
        {
            XmlDocument doc = new XmlDocument();
            doc.Load("c:\\temp\\flickrRepositoryGlobal.xml");
            var data = new List<IStat>();

            // cycle through each child noed
            foreach (XmlNode node in doc.DocumentElement.ChildNodes)
            {
                IStat s = new Stat();
                DateTime p;
                DateTime.TryParse(node.Attributes["Date"].Value, out p);
                s.date = p;
                s.views = Int32.Parse(node.Attributes["Views"].Value);
                s.comments = Int32.Parse(node.Attributes["Comments"].Value);
                s.likes = Int32.Parse(node.Attributes["Likes"].Value);
                data.Add(s);
            }

            var list = new List<SingleStats>();

            for (int i=1; i< data.Count; i++)
            {
                SingleStats stat = new SingleStats() { Date = data[i].date.ToString("d-MMM-yyyy") };
                if (view == "views")
                    stat.Value = data[i].views - data[i-1].views;
                else if (view == "likes")
                    stat.Value = data[i].likes - data[i-1].likes;
                else if (view == "comments")
                    stat.Value = data[i].comments - data[i-1].comments;
                list.Add(stat);
            }

            return Json(new { stats = list }, JsonRequestBehavior.AllowGet);
        }
    }
}