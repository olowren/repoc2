import qgis
from qgis.core import (
    QgsProject,
    QgsVectorLayer,
    QgsCategorizedSymbolRenderer,
    QgsSymbol,
    QgsMarkerSymbol,
    QgsLineSymbol,
    QgsRendererCategory,
    QgsWkbTypes
)
from qgis.utils import iface
from PyQt5.QtGui import QColor

# Couleurs correspondant aux longueurs des itemsets 
COLORS = ['#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4', '#FFEAA7', '#DDA0DD', '#A7C4B6']
COLOR_COLUMN = 'color' # Le nom de la colonne qui contient la couleur 
LENGTH_COLUMN = 'length' # Le nom de la colonne de la longueur de l'itemset

def apply_color_symbology():
    #On fait le chargement de la symbologie sur la couche sélectionnée/active
    layer = iface.activeLayer()
    
    if not layer:
        iface.messageBar().pushMessage("Err", "Aucune couche n'est sélectionnée.", level=qgis.core.Qgis.Warning)
        return
        
    if not isinstance(layer, QgsVectorLayer):
        iface.messageBar().pushMessage("Err", "La couche sélectionnée n'est pas une couche vectorielle.", level=qgis.core.Qgis.Warning)
        return
    
    # Vérification que le fichier contient bien les colonnes de couleur et de longueur d'item
    field_names = [field.name() for field in layer.fields()]
    if COLOR_COLUMN not in field_names or LENGTH_COLUMN not in field_names:
        iface.messageBar().pushMessage("Err", f"Les colonnes '{COLOR_COLUMN}' ou '{LENGTH_COLUMN}' n'existent pas dans la couche.", level=qgis.core.Qgis.Critical)
        return

    #Dictionnaire pour stocker ces informations 
    color_map = {}
    for f in layer.getFeatures():
        color_value = f.attribute(COLOR_COLUMN)
        length_value = f.attribute(LENGTH_COLUMN)
        if color_value not in color_map:
            color_map[color_value] = length_value

    #la géométrie est en wbb
    geometry_type = layer.wkbType()
    
    #catégories pour les couleurs 
    categories = []
    for color_value, length_value in color_map.items():
        symbol_color = QColor(color_value)
        
        if geometry_type in (QgsWkbTypes.Point, QgsWkbTypes.MultiPoint):
            symbol = QgsMarkerSymbol()
            symbol.setColor(symbol_color)
            symbol.setSize(4) # Taille du point
            
        elif geometry_type in (QgsWkbTypes.LineString, QgsWkbTypes.MultiLineString):
            symbol = QgsLineSymbol()
            symbol.setColor(symbol_color)
            symbol.setWidth(1) # Épaisseur de la ligne
            
        else:
            symbol = QgsSymbol.defaultSymbol(geometry_type)
            symbol.setColor(symbol_color)

        category = QgsRendererCategory(color_value, symbol, f'Longueur : {length_value}')
        categories.append(category)

    #Applique le style 
    renderer = QgsCategorizedSymbolRenderer(COLOR_COLUMN, categories)
    layer.setRenderer(renderer)
    layer.triggerRepaint()
    iface.messageBar().pushMessage("Ok", f"Symbologie de couleur appliquée avec succès à la couche '{layer.name()}'.", level=qgis.core.Qgis.Success)

apply_color_symbology()