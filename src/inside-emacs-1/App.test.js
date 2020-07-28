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
#+END_SRC`;


  it('renders the 3 tricks of the content', () => {
    const ast = parse(content);
    const { getByText } = render(
      <App ast={ast} />
    );
    const headerContent = getByText(/Headline 1.1.1/i);
    expect(headerContent).toBeInTheDocument();
  });
})
