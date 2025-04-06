
import pandas as pd
import dash
from dash import Dash, dcc, html, dash_table
from dash.dependencies import Input, Output
import plotly.express as px
import dash_bootstrap_components as dbc

# Cargar datos
file_path = "datos.csv"  # Reemplaza con la ruta real
df = pd.read_csv(file_path)

# Convertir fechas
if 'fecha_divulgada' in df.columns:
    df['fecha_divulgada'] = pd.to_datetime(df['fecha_divulgada'])
    df['Año'] = df['fecha_divulgada'].dt.year

# Inicializar la app con Bootstrap
app = Dash(__name__, external_stylesheets=[dbc.themes.FLATLY])
app.title = "Dashboard_IFC"

# Gráficos 
fig_inversion = px.box(df, x="industria", y="total_inversion_ifc_aprobada_junta_millones_usd",
                        title="Distribución de inversión IFC por industria",
                        labels={'industria': "Industria", 'total_inversion_ifc_aprobada_junta_millones_usd': "Inversión IFC aprobada (millones USD)"})

fig_categoria = px.pie(df, names="categoria_ambiental", values="total_inversion_ifc_aprobada_junta_millones_usd",
                        title="Distribución de inversiones por categoría ambiental")

fig_industria = px.bar(df['industria'].value_counts().reset_index(), 
                       x='count', y='industria', 
                       orientation='h',
                       title="Cantidad de proyectos por industria",
                       labels={'count': "Número de proyectos", 'industria': "Industria"})


fig_hist_inversion = px.histogram(df, x="total_inversion_ifc_aprobada_junta_millones_usd", nbins=30, 
                                  title="Distribución de la Inversión Total IFC Aprobada",
                                  labels={'total_inversion_ifc_aprobada_junta_millones_usd': "Inversión (millones USD)", 'count': "Frecuencia"},
                                  marginal="rug")

# Tabla interactiva
table = dash_table.DataTable(
    id='tabla_ifc',
    columns=[{"name": i, "id": i} for i in df.columns],
    data=df.to_dict('records'),
    page_size=10,
    filter_action="native",
    sort_action="native",
    style_table={'overflowX': 'auto'}
)

# Layout
app.layout = dbc.Container([
    html.H1(" Tendencias y Evolución de los Proyectos de Servicios de Inversión del IFC", className="text-center mt-4 mb-4"),

    dcc.Tabs([
        dcc.Tab(label='Resumen de Inversiones', children=[
            dbc.Row([
                dbc.Col(dcc.Graph(figure=fig_inversion), md=6),
                dbc.Col(dcc.Graph(figure=fig_categoria), md=6)
            ]),
            dbc.Row([
                dbc.Col(dcc.Graph(figure=fig_industria), md=6),
                dbc.Col(dcc.Graph(figure=fig_hist_inversion), md=6)
            ]),
            dbc.Row([
                dbc.Col(dcc.Graph(figure=px.line(df.groupby('Año').size().reset_index(name='Proyectos'),
                                                 x='Año', y='Proyectos', title="Proyectos Aprobados por Año")), md=6),
                dbc.Col(dcc.Graph(figure=px.bar(df.groupby('estado')['total_inversion_ifc_aprobada_junta_millones_usd'].sum()
                                                 .reset_index().sort_values(by='total_inversion_ifc_aprobada_junta_millones_usd', 
                                                 ascending=False),
                                                 x='estado', y='total_inversion_ifc_aprobada_junta_millones_usd',
                                                 title="Estado del Proyecto vs. Monto Invertido",text_auto=True,
                                  labels={'estado': 'Estado del Proyecto', 
                                          'total_inversion_ifc_aprobada_junta_millones_usd': 'Monto Invertido (Millones USD)'})), md=6)
            ])
        ]),

        dcc.Tab(label='Análisis por País', children=[
            html.Label("Ordenar por:", className="mt-3"),
            dcc.RadioItems(
                id='orden_paises',
                options=[
                    {'label': 'Top 10 (Mayor a Menor)', 'value': 'desc'},
                    {'label': 'Bottom 10 (Menor a Mayor)', 'value': 'asc'}
                ],
                value='desc',
                inline=True
            ),
            dcc.Graph(id='fig_pais')
        ]),

        dcc.Tab(label='Tabla de Datos', children=[table])
    ])
], fluid=True)

# Callback para actualizar el gráfico de países según la opción seleccionada
@app.callback(
    Output('fig_pais', 'figure'),
    Input('orden_paises', 'value')
)
def actualizar_fig_pais(orden):
    paises = df.groupby("pais")["total_inversion_ifc_aprobada_junta_millones_usd"].sum()
    
    if orden == 'desc':
        top_paises = paises.nlargest(10).reset_index()
        titulo = "Top 10 Países con Mayor Inversión Aprobada"
    else:
        top_paises = paises.nsmallest(10).reset_index()
        titulo = "Top 10 Países con Menor Inversión Aprobada"

    fig = px.bar(top_paises, x="pais", y="total_inversion_ifc_aprobada_junta_millones_usd",
                 title=titulo,
                 labels={'total_inversion_ifc_aprobada_junta_millones_usd': "Inversión Total (millones USD)", 'pais': "País"},
                 color="total_inversion_ifc_aprobada_junta_millones_usd",
                 color_continuous_scale="viridis")
    
    return fig

if __name__ == '__main__':
    #app.run_server(debug=True)
    app.run(debug=True)

