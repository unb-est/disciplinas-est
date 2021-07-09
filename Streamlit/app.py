import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
from PIL import Image

st.markdown("## Informações Gerais do Departamento")

image = Image.open('unb_logo.png')
st.sidebar.image(image, use_column_width=True)

st.sidebar.title("Disciplinas da Est")
st.markdown("This application is a Streamlit dashboard to analyze the sentiment of tweets 🐦")

st.sidebar.markdown("This application is a Streamlit dashboard to analyze the sentiment of tweets 🐦")

DATA_URL = ("/Users/gabrielreis/Capacitação/Streamlit/Tweets.csv")

historico_url = ("/Users/gabrielreis/Git/disciplinas-est/Streamlit/historico.csv")

@st.cache(persist=True)
def load_data():
    data = pd.read_csv(DATA_URL)
    data['tweet_created'] = pd.to_datetime(data['tweet_created'])
    return data

data = load_data()

select = st.sidebar.selectbox('Período', ['Histogram', 'Pie chart'], key='1')
sentiment_count = data['airline_sentiment'].value_counts()

sentiment_count = pd.DataFrame({'Sentiment': sentiment_count.index,
                                'Tweets': sentiment_count.values})

if not st.sidebar.checkbox("Hide", True):
    st.markdown("### Number of tweets by sentiment")
    if select == "Histogram":
        fig = px.bar(sentiment_count, x='Sentiment', y='Tweets', 
                     color='Tweets', height=500)
        st.plotly_chart(fig)
    else:
        fig = px.pie(sentiment_count, values='Tweets', names='Sentiment')
        st.plotly_chart(fig)

st.sidebar.subheader("When and where are users tweeting from?")
hour = st.sidebar.slider("Hour of day", 0, 23)
modified_data = data[data['tweet_created'].dt.hour == hour]

if not st.sidebar.checkbox("Close", True, key='1'):
    st.markdown("### Tweets location based on time of day")
    st.markdown("%i tweets between %i:00 and %i:00" % (len(modified_data), hour, (hour+1)%24))
    st.map(modified_data)
    if st.sidebar.checkbox("Show raw data", False):
        st.write(modified_data)
# two conditions must be met: data must have columns named latitude and longitude (or lat and long)
# the two columns can't have missing data

st.sidebar.subheader("Breakdown airline tweets by sentiment")
choice = st.sidebar.multiselect('Pick airline', 
                                ('US Airways', 'United', 'American', 'American', 'Southwest', 'Delta', 'Virgin America'), key='0')

if len(choice) > 0:
    choice_data = data[data.airline.isin(choice)]
    fig_choice = px.histogram(choice_data, x='airline', y='airline_sentiment', 
                              histfunc='count', color='airline_sentiment',
                              facet_col='airline_sentiment', labels={'airline_sentiment':'tweets'}, height=600, width=800)
    st.plotly_chart(fig_choice)
    # histfunc to perform computation on the y value

# WordCloud
from wordcloud import WordCloud, STOPWORDS
import matplotlib.pyplot as plt # pra tirar os eixos
st.set_option('deprecation.showPyplotGlobalUse', False)

st.sidebar.header("Word Cloud")
word_sentiment = st.sidebar.radio('Display word cloud for what sentiment?', ('positive', 'neutral', 'negative'))

if not st.sidebar.checkbox("Hide", True, key='3'):
    st.subheader('Word Cloud for %s sentiment' % (word_sentiment))
    df = data[data['airline_sentiment'] == word_sentiment]
    words = ' '.join(df['text'])
    processed_words = ' '.join([word for word in words.split() if 'http' not in word and not word.startswith('@') and word != 'RT'])
    wordcloud = WordCloud(stopwords=STOPWORDS, background_color='white', height=640, width=800).generate(processed_words)
    plt.imshow(wordcloud)
    plt.xticks([])
    plt.yticks([])
    st.pyplot()

#%%
# import pandas as pd
# dados =pd.read_csv("/Users/gabrielreis/Capacitação/Streamlit/Tweets.csv")
# dados.head()