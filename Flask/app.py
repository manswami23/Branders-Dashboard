from flask import Flask, render_template
app = Flask(__name__)

@app.route('/')
def home():
    return render_template("home.html")

@app.route('/analytics.html')
def analytics():
    return render_template("analytics.html")

@app.route('/news-media.html')
def nm():
    return render_template("news-media.html")