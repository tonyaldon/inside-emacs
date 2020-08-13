import React from "react";

/** Walk in the AST tree (obtain with orga) and goto a specific node.*/
function astGoToNode(ast, branches) {
  var node = ast;
  if (branches == null) {
    return node;
  }
  for (let branch of branches) {
    node = node.children[branch];
  }
  return node;
}

/** From nested list of nodes of type "list", "list.item",
    "headline" and "block" in the AST tree (obtain with "orga"),
    filters and returns an array of nodes only of the types
    "list.item" or "block". */
function astMakeListItemBlock(astList) {
  // var children = astGoToNode(astTopSection, [1,1]).children;
  var listItemBlock = [];
  for (var node = 0; node < astList.length; node++) {
    switch (astList[node].type) {
    case 'block':
      listItemBlock.push(astList[node]);
      break;
    case 'list':
      let itemList = astList[node].children;
      for (var item = 0; item < itemList.length; item++) {
        listItemBlock.push(itemList[item]);
      }
      break;
    default:
    }
  }
  return listItemBlock;
}

/** From a top section in the AST tree (obtain with orga) and the trick
    index ([2,1] for second headline at level 2 and first headline at level 3)
    in that top section returns a trick. **/
function astMakeTrick(astTopSection, trickIndex) {
  const headline2Index = trickIndex[0];
  const headline3Index = trickIndex[1];
  return {
    headline_1 : astGoToNode(astTopSection, [0,0]).value,
    headline_2 : astGoToNode(astTopSection, [headline2Index,0,0]).value,
    headline_3 : astGoToNode(astTopSection, [headline2Index,
                                             headline3Index,0,0]).value,
    list : astMakeListItemBlock(
      astGoToNode(astTopSection,[headline2Index,headline3Index]).children)
  };
}

/** Make the list off all tricks of the AST tree (obtain with orga).*/
function astMakeTricks(ast) {
  let tricks = [];
  ast.children.forEach((astTopSection) => {
    astTopSection.children.slice(1).forEach((headline2,headline2Index) => {
      headline2.children.slice(1).forEach((headline3,headline3Index) => {
        let trickIndex = [headline2Index + 1, headline3Index + 1];
        tricks.push(astMakeTrick(astTopSection, trickIndex));
      })
    })
  })
  return tricks
}

/** From a node AST tree (obtain with orga) returns its text
    content wrapped with the appropriate html tag.*/
function MarkupOrLink(props) {
  const astNode = props.node;
  let element;
  switch (astNode.type) {
  case 'text':
    element = astNode.value;
    break;
  case 'link':
    element = <a href={astNode.uri.raw}>{astNode.desc}</a>
    break;
  case 'strikeThrough':
    element = <del>{astNode.children[0].value}</del>;
    break;
  case 'underline':
    element = <span style={{textDecoration: 'underline'}}>
                {astNode.children[0].value}
              </span>;
    break;
  case 'bold':
    element = <b>{astNode.children[0].value}</b>;
    break;
  case 'italic':
    element = <i>{astNode.children[0].value}</i>;
    break;
  case 'code':
    element = <code>{astNode.children[0].value}</code>;
    break;
  case 'verbatim':
    element = <code>{astNode.children[0].value}</code>;
    break;
  default:
  }
  return element;
}

export { astGoToNode, astMakeListItemBlock, astMakeTrick,
         astMakeTricks, MarkupOrLink }
