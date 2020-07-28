it('Makes "strikeThrough" markup', () => {
  const content = `+strikeThrough+`;
  const ast = parse(content);
  const node = astGoToNode(ast, [0, 0]);
  const { container } = render(<MarkupOrLink node={node} />);
  expect(container.firstChild).toMatchInlineSnapshot(`
      <del>
        strikeThrough
      </del>
    `);
})
