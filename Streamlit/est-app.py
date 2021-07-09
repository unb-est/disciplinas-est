import streamlit as st
from PIL import Image
from multiapp import MultiApp
from paginas import bacharelado, geral, professores, servico

app = MultiApp()

image = Image.open('unb_logo.png')
st.sidebar.image(image, use_column_width=True)

st.markdown("""
# Departamento de Estatística - UnB
""")

# Adicionar páginas
app.add_app("Informações Gerais", geral.app)
app.add_app("Disciplinas do Bacharelado", bacharelado.app)
app.add_app("Disciplinas de Serviço", servico.app)
app.add_app("Professores", professores.app)

# App Principal
app.run()