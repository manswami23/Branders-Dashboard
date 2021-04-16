from flask import Flask, render_template, request, redirect, url_for
import tweepy
import KeysAndTokens

app = Flask(__name__)

# twitter authentication
# https://twitter.com/tester3216
auth = tweepy.OAuthHandler(KeysAndTokens.API_KEY, KeysAndTokens.API_KEY_SECRET)
auth.set_access_token(KeysAndTokens.ACCESS_TOKEN, KeysAndTokens.ACCESS_TOKEN_SECRET)
twitterApi = tweepy.API(auth)

@app.route('/')
def home():
    return render_template("home.html")

@app.route('/analytics.html')
def analytics():
    return render_template("analytics.html")

@app.route('/news-media.html', methods=['GET', 'POST'])
def nm():

    if request.method == "POST":
        if len(request.form['caption']) > 0 and len (request.form) > 1:
            caption = request.form['caption']
            if request.form.get('Twitter', "") == 'on' and request.form.get('Facebook', "") == 'on':
                print("both")
            elif request.form.get('Twitter', "") == 'on':
                twitterApi.update_status(caption)
            elif request.form.get('Facebook', "") == 'on':
                print("fb")
            else:
                print("invalid")
                redirect(url_for('nm'))
            
            print(request.form)  

    return render_template("news-media.html")

if __name__ == '__main__':
    app.run('localhost', 5000)
