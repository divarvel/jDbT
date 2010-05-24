#!/usr/bin/python
# -*- coding: utf-8 -*- 



"""DB model Generator
Options:
  jDbT [-vh] [-Y yaml_output_file]  [-X xml_output_file]  [-p pgSQL_output_file]  [-m MySQL_output_file] [-g graphviz_output_file] [-G png_output_file] [-T type] input_file

Sample Input data:
table:
  table_foo:
  anotherTable_id:
anotherTable:
  anotherTable_bar:
  
Primary Keys are automatically added (tableName_id)
Foreign Keys are inferred from the fields names (anotherTable_id)
"""

import os
import sys
import getopt
import yaml
import copy
import types

conf = {}
conf["datatype"] = {"pgsql" : {} }
conf["datatype"]["pgsql"]["varchar"] = {'propel_type' : 'VARCHAR(EXLENGTH)' , 'default_length' : '100', 'sql_type' : 'VARCHAR(EXLENGTH)'}
conf["datatype"]["pgsql"]["integer"] = {'propel_type' : 'INTEGER', 'sql_type' : 'INTEGER' }




class Options:
  __single = None
  "Modification du comportement par défaut du programme"
  def __init__(self):
    if Options.__single:
      raise Options.__single
    else :
      Options.__single = self
      self.verbose = False
      self.stream_type = False
      self.yml_file = False
      self.xml_file = False
      self.pgsql_file = False
      self.mysql_file = False
      self.graphviz_file = False
      self.png_file = False
      self.yml_file = False
      self.input = ""
      self.symfony = False
      self.cluster= True
  def process(self):
  # parse command line options
    try:
	    opts, args = getopt.getopt(sys.argv[1:], "Y:X:p:m:g:G:T:svh", ["yml:", "xml:", "pgsql:", "mysql:", "graphviz:", "graphviz-image:", "type:", "verbose", "help","symfony","ignore-cluster"])
  
    except getopt.error, msg:
      print msg
      print "for help use --help"
      sys.exit(2)
      # process options
    for o, a in opts:
      if o in ("-h", "--help"):
        print __doc__
        sys.exit(0)
      if o in ("-s", "--symfony"):
        self.symfony = True
      
      if o in ("--ignore-cluster"):
        self.cluster = False
        
      if o in ("-Y", "--yml"):
        if a != "" :
          self.yml_file = a
      
      if o in ("-X", "--xml"):
        if a != "" :
          self.xml_file = a
      
      if o in ("-p", "--pgsql"):
        if a != "" :
          self.pgsql_file = a
      
      if o in ("-m", "--mysql"):
        if a != "" :
          self.mysql_file = a
      
      if o in ("-g", "--graphviz"):
        if a != "" :
          self.graphviz_file = a
      
      if o in ("-G", "--graphviz-image"):
        if a != "" :
          self.png_file = a
      
      if o in ("-T", "--type"):
        if a in ("yml", "xml", "pgsql", "mysql", "graphviz"):
          self.stream_type = a
      
      if o in ("-v", "--verbose"):
        self.verbose = True
  
    # process arguments
    if len(args) >= 1 :
      for arg in args:
        self.input = arg
    else :
      print __doc__
      sys.exit(2)


def debug(string):
  #options = Options()
  if options.verbose:
    print string
  


class Table:
  "Définition d'une table de données"
  def __init__(self, name=""):
    self.fieldlist = []
    self.name = name
    self.props = {}
  def add_field(self,field):
    self.fieldlist.append(field)
  
  def to_ymlDict(self):
    result = {}
    result[self.name] = self.props
    for field in self.fieldlist:
      result[self.name].update(field.to_ymlDict())
    return result
  
  def __str__(self):
    str =  '  '+self.name + ':'
    for field in self.fieldlist:
      str += '\n    ' + field.__str__()
    return str
  def add_attributes(self,dict):
    self.cluster = dict.setdefault("cluster","zero")
    dict.pop("cluster")
    if dict != {} :
      self.add_props(dict,"_attributes")
  
  def add_props(self,dict,propname):
    self.props[propname] = dict
  
  def set_pk(self):
    #TODO Check wether the code below actually works
    pfdefined = False
    for field in self.fieldlist:
      if field.__dict__.has_key("primaryKey"):
        pkdefined = True
        return False
    dict = {'type' : 'integer' , 'required' : True ,  'primaryKey' : True , 'autoIncrement' : True}
    self.add_field(Field(name=self.name+'_id' ,tablename=self.name,dict=dict))
    self.fieldlist.reverse()
  def sort_fields(self):
    dictpk = {}
    dictfk = {}
    dictatt = {}
    for field in self.fieldlist:
      if field.__dict__.has_key("primaryKey"):
        dictpk[field.rendername()] = field
      elif field.__dict__.has_key("foreignTable"):
        dictfk[field.rendername()] = field
      else:
        dictatt[field.rendername()] = field
    dicts = [dictpk , dictfk, dictatt]
    result = []
    for dict in dicts:
      keys = dict.keys()
      keys.sort()
      for key in keys:
        result.append(dict[key])

    self.fieldlist = result
 
 
 #######
 ###------ Field
 
class Field:
  "Définition d'un champ de données"
  def __init__(self, name="",tablename="",dict={}):
    if dict == None:
      dict= {}
    if type(dict) != types.DictType:
      stringtype = str(dict)
      dict = {}
      if stringtype[0] == '+':
        dict["required"] = True
        stringtype = len(stringtype) == 0 and "" or stringtype[1:]
      if len(stringtype)>0:
        dict["type"] = stringtype
    self.__dict__ = dict
    self.name = name
    self.tablename=tablename
    self.type = not self.__dict__.has_key("type") and "varchar(100)" or self.type
  def set_foreign(self):
    if '_id' == self.name[-3:] and not self.__dict__.has_key("foreignTable"): 
      self.type = "integer"
      self.foreignTable = self.name[0:-3]
      self.foreignReference = self.name
  
  def rendername(self):
    if self.name.find('_')<0:
      return self.tablename+'_'+self.name
    return self.name
  
  def rendertype(self, data_type):
    type = ""
    if data_type == "mysql":
      if self.__dict__.has_key('primaryKey') and self.primaryKey == 'true':
        type = "INTEGER  NOT NULL AUTO_INCREMENT"
      elif type == "timestamp":
        type = "DATETIME"
      else:
        type = self.type.upper()
    if data_type == "pgsql":
      type = self.type
    return type
    
  def to_ymlDict(self):
    result = {}
    dict = copy.copy(self.__dict__)
    dict.pop("name")
    dict.pop("tablename")
    result[self.rendername()] = dict
    return result
    
  def __str__(self):
    dict = copy.copy(self.__dict__)
    dict.pop("name")
    dict.pop("tablename")
    str =  self.rendername() + ": {"
    comma = False
    for attname in dict.keys():
      if dict[attname] != None:
        if comma :
          str += ', '
        else :
          comma = True
        str += attname+': '+ dict[attname].__str__()
        
    str += "}"
    return str

class DataBase:
  "Définition de la base de données"
  def __init__(self):
    self.package = "lib.model"
    self.props = {}
  def add_props(self,dict,propname):
    self.props[propname] = dict    
  
  def initFromSymfonyDict(self,symfonyStruct):
    
    struct = symfonyStruct["propel"]
    #print struct
    self.tablenames = struct.keys()
    self.tablenames.sort()
    self.tables = []
    #print self.tablenames
    for tablename in self.tablenames:
      fieldnames = {}
      if struct[tablename]:
        fieldnames = struct[tablename].keys()
      
      if "_" != tablename[0]:
        table = Table(name=tablename)
        self.tables.append(table)
        debug('table:'+tablename)
        for fieldname in fieldnames:
          debug('--field:'+fieldname)
          if fieldname == "_attributes":
            table.add_attributes(struct[tablename][fieldname])
          elif fieldname[0] == '_':
            table.add_props(struct[tablename][fieldname],fieldname)
          else:
            field = Field(name=fieldname,tablename=tablename,dict=struct[tablename][fieldname])
            field.set_foreign()
            table.add_field(field)
        
        table.set_pk()
        table.sort_fields()
      else:
        self.add_props(copy.deepcopy(struct[tablename]),tablename)
  def render_yml(self):
    result = {}
    result["propel"] = {}
    for table in self.tables:
      #print table.to_ymlDict()
      if self.props.has_key("_behaviors"):
        table.add_props(copy.deepcopy(self.props["_behaviors"]),"_behaviors")
      result["propel"].update(table.to_ymlDict())
    
    # default_flow_style is at False due to malformed string for symfony
    return yaml.dump(result, default_flow_style=False)
  
  def render_yml_old(self):
    output = "propel:\n"
    for table in self.tables:
      output += table.__str__() + "\n"
    return output
  
  def render_xml(self):
    output = ""
    return output
    
  def render_pgsql(self):
    output = ""
    for table in self.tables:
      output += 'CREATE TABLE '+table.name+'();\n'
      for field in table.fieldlist:
        output += '  ALTER TABLE '+table.name+' ADD COLUMN '+field.rendername()+' '+field.rendertype("pgsql")+' ;\n'
        if field.__dict__.has_key('primaryKey') and field.primaryKey == 'true':
          output += '    CREATE SEQUENCE '+ table.name + '_' + field.rendername() + '_seq  INCREMENT 1   MINVALUE 1  Start 1   CACHE 1;\n'
          output += '    ALTER TABLE '+table.name+' ALTER COLUMN '+field.rendername()+' SET DEFAULT nextval(\''+table.name + '_' + field.rendername()+'_seq\') ;\n'
          output += '     ALTER TABLE '+table.name+' ADD CONSTRAINT pk_'+table.name+' PRIMARY KEY('+field.rendername()+');\n'
      output += '\n'
    
    for table in self.tables:
      output += '\n'
      for field in table.fieldlist:
        if field.__dict__.has_key("foreignTable"):
          output += '  ALTER TABLE '+table.name+' ADD CONSTRAINT fk_'+table.name+'_'+field.foreignTable+' FOREIGN KEY ('+field.rendername()+')  REFERENCES '+field.foreignTable+' ('+field.foreignReference+')  MATCH SIMPLE ON UPDATE CASCADE ON DELETE CASCADE;\n'
    return output
  
  def render_mysql(self):
    output = ""
    for table in self.tables:
      output += '#### TABLE ' + table.name + ' ####\n'
      output += 'DROP TABLE IF EXISTS`'+table.name+'`;\n\n'
      output += 'CREATE TABLE `'+table.name+'`\n(\n'
      for field in table.fieldlist:
        output += '    `'+field.rendername()+'` '+field.rendertype("mysql")+',\n'
      for field in table.fieldlist:
        if field.__dict__.has_key('primaryKey') and field.primaryKey == 'true':
          output += '    PRIMARY KEY (`'+ field.rendername() +'`)\n'
      output += ');\n\n\n'

    for table in self.tables:
      output += '\n'
      for field in table.fieldlist:
        if field.__dict__.has_key("foreignTable"):
            output += 'ALTER TABLE `'+table.name+'` ADD CONSTRAINT `fk_'+table.name+'_'+field.foreignTable
            output += ' FOREIGN KEY (`'+field.rendername()
            output += ' REFERENCES '+field.foreignTable+' ('+field.foreignReference+');\n'

    
    return output
    
  def render_graphviz(self):
    output = 'digraph g {\n\
  graph [rankdir = "BT"];\n\
  node [FontSize = "16"];\n\n'
    
    # Display Tables
    for table in self.tables:
      for field in table.fieldlist: # Display relations
        if field.__dict__.has_key("foreignTable"):
          output +=  "  " + table.name + " -> "+ field.foreignTable + '; \n'
    
    output += "\n"
    for table in self.tables:
      output += "  " + table.name + '[ label = <<TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0" WIDTH="100"> \n\
  <TR><TD BORDER="1" BGCOLOR="#BBBBBB" WIDTH="100">'+table.name+'</TD></TR>\n'
      for field in table.fieldlist:
        if field.__dict__.has_key("primaryKey"): # PK highlighting
          output += '  <TR><TD ALIGN="LEFT" BGCOLOR="#FFCCCC" WIDTH="100">'
        elif field.__dict__.has_key("foreignTable"): # FK highlighting
          output += '  <TR><TD ALIGN="LEFT" BGCOLOR="#CCFFCC" WIDTH="100">'
        else:
          output += '  <TR><TD ALIGN="LEFT" WIDTH="100">'
        output += field.rendername() + '</TD></TR>\n'
      output += '  </TABLE>> shape = "plaintext" ];\n\n'
    
    
    if not options.cluster:
      output += "}"
      return output
    
    clusters = {}
    
    
    
    
    # Get the clusters list
    for table in self.tables:
      clustername = table.__dict__.setdefault("cluster","zero")
      if not clusters.has_key(clustername):
        cluster = []
        clusters[clustername] = cluster
      else:
        cluster = clusters[clustername]
      cluster.append(table.name)
    
    # Display Clusters
    for key in clusters.keys():
      output += '  subgraph cluster_' + key + ' {\n    label = '+key + ';\n'
      for tablename in clusters[key]:
        output += '    ' + tablename + ';\n'
      
      output += '  }\n'
      
    
    output += "}"
    return output


if __name__ == '__main__':
  options = Options()
  options.process()
  # Import YAML file
  data = open(options.input, "r")
  tempstruct = yaml.load(data)
  data.close()
  struct = tempstruct
  if not options.symfony:
    struct = {}
    struct["propel"] = tempstruct
    
    
  
  database = DataBase()
  database.initFromSymfonyDict(struct)
  
  if options.stream_type == "yml":
    print database.render_yml()
    
  if options.stream_type == "xml":
    print database.render_xml()
  
  if options.stream_type == "pgsql":
    print database.render_pgsql()
    
  if options.stream_type == "mysql":
    print database.render_mysql()
  
  if options.stream_type == "graphviz":
    print database.render_graphviz()
    
  if options.yml_file:
    file = open(options.yml_file, "w")
    file.write(database.render_yml())
    file.close()
  
  if options.xml_file:
    file = open(options.xml_file, "w")
    file.write(database.render_xml())
    file.close()
  
  if options.pgsql_file:
    file = open(options.pgsql_file, "w")
    file.write(database.render_pgsql())
    file.close()
  
  if options.mysql_file:
    file = open(options.mysql_file, "w")
    file.write(database.render_mysql())
    file.close()    
  if (options.png_file or options.graphviz_file):
    options.graphviz_file = not options.graphviz_file and ".tmp.dot~" or options.graphviz_file
    file = open(options.graphviz_file, "w")
    file.write(database.render_graphviz())
    file.close()
    
    if options.png_file:
      os.system("dot -Tpng -o "+options.png_file+" "+options.graphviz_file)
    
    if(options.graphviz_file == ".tmp.dot~"):
      os.remove(options.graphviz_file)

