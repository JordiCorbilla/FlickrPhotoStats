FlickrPhotoStats
================

Flickr Photo stats provides you with the ability to request information from your Flickr stream using the Web REST API.

Download the latest version here:

[![Downloads](https://img.shields.io/badge/downloads-1k-blue.svg)](https://app.box.com/s/sfjwsbrywmoo6v1obfi62fjcbpw7wyge) [![Stable Release](https://img.shields.io/badge/version-4.5.0.29rc-blue.svg)](https://app.box.com/s/sfjwsbrywmoo6v1obfi62fjcbpw7wyge) [![License](https://img.shields.io/badge/license-GPL-blue.svg)]https://app.box.com/s/sfjwsbrywmoo6v1obfi62fjcbpw7wyge) [![Delphi version](https://img.shields.io/badge/delphi-xe6-red.svg)](https://app.box.com/s/sfjwsbrywmoo6v1obfi62fjcbpw7wyge)

**Additional libraries required:**
The application requires SSL authentication and for that it uses the libraries provided by OpenSSL.
Remember to download the following libraries and place them in the application root:

- [OpenSSL v1.02 32bits](http://indy.fulgan.com/SSL/openssl-1.0.2d-i386-win32.zip).
- [OpenSSL v1.02 64bits](http://indy.fulgan.com/SSL/openssl-1.0.2d-x64_86-win64.zip).

**License:** GNU General Public License.

The app only needs a valid *API Key* that can be requested in the App Garden where my application is already [published](https://www.flickr.com/services/apps/72157639602915254/):

[The App Garden - API request](https://www.flickr.com/services/apps/create/apply/?).

Once obtained, you can load your list or start a new one using the Api Key.

Additional info can be found in the following [**post**](http://thundaxsoftware.blogspot.com/p/flickr-photo-analytics-v44.html) in my blog.

**Example of the app**:

**Dashboard**:

![](http://2.bp.blogspot.com/-pOsbaNlGn8A/VdzVz5L3CJI/AAAAAAAAE_U/TSorHxtJO40/s640/version.png)

**Processing**:

![](http://3.bp.blogspot.com/-L0tJYC6NnH4/VdzWXMZCCcI/AAAAAAAAE_c/HYShnUDk0TY/s640/proces.png)

**Authentication**:

![](http://2.bp.blogspot.com/-buVw7akFPG4/VdzYG7JkCcI/AAAAAAAAE_w/qolCi6kzKDY/s640/auth2.png)

It will keep a list of all the photos you add to keep track of them automatically.
The following information will be saved:
- Number of views.
- Number of Likes.
- Number of Comments.
- Last time it was updated.
- Affection (number of likes over number of views).

Each value is saved and stored over time so the tool can display the trend of a particular photo.

Code convention:
- > 1000 < 3000 views: blue.
- > 3000 < 5000 views: green.
- > 5000 < 8000 views: Olive.
- > 8000 < 10000 views: Fuchsia. 
- > 10000 views: red.
 
Use **Batch Update** to easily update all the values and keep a good track record of your flickr stream.

Version 3.0 (stable):

- [FlickrPhotoStats(x86) v3.0](https://app.box.com/s/v3xdczujjdm1b85kcs8k). 
- [FlickrPhotoStats(x64) v3.0](https://app.box.com/s/1i67o9g6krr0398kprk1).

Version 3.1 (RC):

- [FlickrPhotoStats(x86) v3.1](https://app.box.com/s/djm6lei8rli3pkfa1fy3).
- [FlickrPhotoStats(x64) v3.1](https://app.box.com/s/w24hr4cq8nkh9c22z4iw).

Version 4.1 (beta):

- [FlickrPhotoStats(x86) v4.1](https://app.box.com/s/ydtsu1ceq3f5dkkilct20qhnkdahcddd).
- [FlickrPhotoStats(x64) v4.1](https://app.box.com/s/x3ta5x4z3o6e5h7ozc82d6e49uslzjw5).

Version 4.3 (RC):

- [FlickrPhotoStats(x64) v4.3](https://app.box.com/s/3ftq5jts9srydqzwpj02adml1gkav435).

Version 4.4 (RC):

- [FlickrPhotoStats(x64) v4.4](https://app.box.com/s/qpe03ssodxwj55orhhzmd7xpms21qvd7).
- [FlickrPhotoStats(x64) v4.4.0.82](https://app.box.com/s/1l6otaof0iefazqt8tp1602bv98bji9r).

Latest release candidate can be found here:

- [FlickrPhotoStats(x64) v4.5.0.29](https://app.box.com/s/sfjwsbrywmoo6v1obfi62fjcbpw7wyge).

## Changelog
### Features for version 3.1:
- Get the total list of photos using flickr.people.getPhotos for automatic population of the application.

### Features for version 4.1:
- OAuth authentication.
- Get list of groups and create your own profiles for automatic population.
- Totals for Albums.

### Features for version 4.3:
- Dashboard.
- Trend lines.
- Album population.

### Features for version 4.4:
- Dashboard upgrade.
- Area series.
- Performance improvements.
- BatchUpdate to send emails out to users.
- emails support HTML.
- splash screen.
- Photo filtering.

### Features for version 4.5:
- Dashboard upgrade.
- Double click on graphs to expand.
- Performance improvements.
- BatchUpdate to send emails out to users.
- emails support HTML.
- splash screen.
- Photo filtering.
- Memory leaks.
- Web dashboard using ASP.NET MVC.

Source code is available here.

The application can now use **OAuth** authentication using REST. The application itself can validate the user tokens and provide all the flow within the application. I will write a post about this in my blog.

Developed with Delphi XE6 and C# and using [OpenSSL](https://www.openssl.org/) libraries..

## Roadmap
### Version 4.4:
- Additional photo filtering.
- Speed improvements.

### Version 4.5:
- Authentication improvements.
- Pool improvements.

### Version 5:
- Saving the repository in a SQL DB (MySQL, SQL Server).
- For version 5 I will start my hosted version where I will be able to take users on-board and just email the results back.

## Sponsors
No sponsors yet! Will you be the first?

[![PayPayl donate button](https://img.shields.io/badge/paypal-donate-yellow.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=L5FCF6LX5C9AW "Donate once-off to this project using Paypal")
