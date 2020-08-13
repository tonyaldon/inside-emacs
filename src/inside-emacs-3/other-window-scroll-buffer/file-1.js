import React from 'react'
import './App.scss'
import { astMakeTricks, MarkupOrLink } from './astUtils.js'
import { Collapse } from 'react-collapse'

function NavBar() {
  return (
    <nav className="navbar nav-trick" role="navigation" aria-label="main navigation">
      <div className="navbar-brand">
        <div className="navbar-item">
          <div className="nav-brand">
            <span className="nav-brand-name">
              Tricks
            </span>
            <span className="nav-brand-author">
              by Tony Aldon
            </span>
          </div>
        </div>
      </div>
      <div className="navbar-menu">
        <div className="navbar-end">
          <a className="navbar-item" href="https://twitter.com/tonyaldon">
            <span className="icon is-small">
              <i className="mdi mdi-twitter"></i>
            </span>
          </a>
          <a className="navbar-item" href="https://github.com/tonyaldon/tonyaldon.github.io">
            <span className="icon is-small">
              <i className="mdi mdi-github"></i>
            </span>
          </a>
        </div>
      </div>
    </nav>
  );
}

function Footer() {
  return (
    <footer className="footer">
      <div className="content">
        <p>Tricks by Tony Aldon</p>
      </div>
    </footer>
  );
}

class Trick extends React.Component {
  constructor(props) {
    super(props);
    this.state = {isOpened: false};
  }

  handleClick = () => {
    this.setState({isOpened: !this.state.isOpened})
  }

  render () {
    const title = this.props.trick.headline_3;
    const nodeList = this.props.trick.list;
    const items = nodeList.map((node,index) => {
      if (node.type === 'list.item') {
        return (
          <li key={index}>
            <Paragraph
              paragraph={node.children}
            />
          </li>
        );
      } else {
        return (
          <Block
            key={index}
            block={node}
          />
        );
      }
    });
    return (
      <div className="trick">
        <div className="trick-button-container">
          <button
            data-testid="trick-title"
            onClick={this.handleClick}
            type="button"
            className="trick-button"
          >
            {title}
          </button>
        </div>
        <Collapse
          isOpened={this.state.isOpened}
          theme={{collapse: 'ReactCollapse--collapse',
                  content: 'content trick-content ReactCollapse--content'}}
        >
          <ul>{items}</ul>
        </Collapse>
      </div>
    );
  }
}

function Block(props) {
  const language = props.block.params[0];
  const code = props.block.value;
  return (
    <pre>
      <code className={language}>{code}</code>
    </pre>
  );
}

function Paragraph(props) {
  const paragraphList = props.paragraph;
  const paragraph = paragraphList.map((node,index) =>
    <MarkupOrLink key={index} node={node} />);
  return (
    <p>{paragraph}</p>
  );
}

function App(props) {
  const ast = props.ast;
  const tricks = astMakeTricks(ast);
  const trickList = tricks.map((trick,index) =>
    <Trick
      key={index}
      trick={trick}
    />);
  return (
    <div className="container">
      <NavBar />
      <ul>{trickList}</ul>
      <Footer />
    </div>
  );
}

export { App, NavBar, Trick, Block, Paragraph }
