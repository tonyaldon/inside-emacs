import React from 'react'
import { render } from '@testing-library/react'
import { screen, fireEvent } from '@testing-library/dom'
import { App, NavBar, Trick, Block, Paragraph } from './App'
import { parse } from 'orga'
import { astMakeTrick } from './astUtils.js'

it('renders header content', () => {
  const { getByText } = render(<NavBar />);
  const headerContent = getByText(/Tricks/i);
  expect(headerContent).toBeInTheDocument();
})

describe('Trick', () => {
  const content = `
* Headline 1
** Headline 1.1
*** Headline 1.1.1
- item 1 ~inline code~ and *bold words*
- item 2
#+BEGIN_SRC bash
some code 1
#+END_SRC
- item 3
** Headline 1.2
*** Headline 1.2.1
- item 1 ~inline code~ and *bold words*
`;

  it('renders "Trick" component collapsed', () => {
    const ast = parse(content);
    const astTopSection = ast.children[0];
    const trickIndex = [1,1];
    const trick = astMakeTrick(astTopSection, trickIndex);
    const { getByText, getByTestId, container } = render(<Trick trick={trick} />);

    expect(getByText(/Headline 1.1.1/i)).toBeInTheDocument();
    expect(getByTestId('trick-title').parentNode.parentNode.childNodes[1])
      .toHaveStyle('overflow: hidden')
  });

  it('shows and hides the content of the "Trick" component', () => {
    const ast = parse(content);
    const astTopSection = ast.children[0];
    const trickIndex = [1,1];
    const trick = astMakeTrick(astTopSection, trickIndex);
    const { getByText, getByTestId, container } = render(<Trick trick={trick} />);

    expect(getByTestId('trick-title').parentNode.parentNode.childNodes[1])
      .toHaveStyle('overflow: hidden')

    fireEvent.click(getByText(/Headline 1.1.1/i));
    expect(getByTestId('trick-title').parentNode.parentNode.childNodes[1])
      .toHaveStyle('overflow: initial')

    fireEvent.click(getByText(/Headline 1.1.1/i));
    expect(getByTestId('trick-title').parentNode.parentNode.childNodes[1])
      .toHaveStyle('overflow: hidden')
  })
});

it('renders "Block" component', () => {
  const content = `
#+BEGIN_SRC javascript
const parser = new Parser()
const ast = parser.parse('Hello World')
#+END_SRC
`;

  const ast = parse(content);
  const block = ast.children[0];
  const { getByText } = render(<Block block={block} />);
  const blockText = getByText(/parse\('Hello World'\)/i);
  expect(blockText).toBeInTheDocument();
})

it('renders "Paragraph" component', () => {
  const content = `
_underline_ and +strikeThrough+ and *bold* and /italic/ and
~code~ and =verbatim= and [[https://github.com/tonyaldon/][link]].
`;

  const ast = parse(content);
  const paragraph = ast.children[0].children;
  const { getByText } = render(<Paragraph paragraph={paragraph} />);
  expect(getByText(/underline/i)).toBeInTheDocument();
  expect(getByText(/strikeThrough/i)).toBeInTheDocument();
  expect(getByText(/bold/i)).toBeInTheDocument();
  expect(getByText(/italic/i)).toBeInTheDocument();
  expect(getByText(/code/i)).toBeInTheDocument();
  expect(getByText(/verbatim/i)).toBeInTheDocument();
  expect(getByText(/link/i)).toBeInTheDocument();
})

describe('App', () => {
  const content = `
* Headline 1
** Headline 1.1
*** Headline 1.1.1
- item 1
- item 2
#+BEGIN_SRC bash
some code 1
#+END_SRC
*** Headline 1.1.2
- item 1
- item 2
* Headline 2
** Headline 2.1
*** Headline 2.1.1
- item 1
- item 2
- item 3
#+BEGIN_SRC bash
some code 2
#+END_SRC
** Headline 2.2
*** Headline 2.2.1
- item 1
#+BEGIN_SRC bash
some code 3
#+END_SRC
`;


  it('renders the 4 tricks of the content', () => {
    const ast = parse(content);
    const { getByText } = render(
      <App ast={ast} />
    );
    const trick_1 = getByText(/Headline 1.1.1/i);
    const trick_2 = getByText(/Headline 1.1.2/i);
    const trick_3 = getByText(/Headline 2.1.1/i);
    const trick_4 = getByText(/Headline 2.2.1/i);
    expect(trick_1).toBeInTheDocument();
    expect(trick_2).toBeInTheDocument();
    expect(trick_3).toBeInTheDocument();
    expect(trick_4).toBeInTheDocument();
  });
})
