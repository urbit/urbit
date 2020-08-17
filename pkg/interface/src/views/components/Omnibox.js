import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { Box, Row, Rule, Text } from '@tlon/indigo-react';
import index from '~/logic/lib/omnibox';
import Mousetrap from 'mousetrap';
import OmniboxInput from './OmniboxInput';
import OmniboxResult from './OmniboxResult';

import { cite } from '~/logic/lib/util';

export class Omnibox extends Component {
  constructor(props) {
    super(props);
    this.state = {
      index: new Map([]),
      query: '',
      results: this.initialResults(),
      selected: ''
    };
    this.handleClickOutside = this.handleClickOutside.bind(this);
    this.search = this.search.bind(this);
    this.navigate = this.navigate.bind(this);
    this.control - this.control.bind(this);
    this.setPreviousSelected = this.setPreviousSelected.bind(this);
    this.setNextSelected = this.setNextSelected.bind(this);
    this.renderResults = this.renderResults.bind(this);
  }

  componentDidUpdate(prevProps) {
    if (prevProps !== this.props) {
      this.setState({ index: index(this.props.associations, this.props.apps.tiles) });
    }

    if (prevProps && this.props.show && prevProps.show !== this.props.show) {
      Mousetrap.bind('escape', () => this.props.api.local.setOmnibox());
      document.addEventListener('mousedown', this.handleClickOutside);
      const touchstart = new Event('touchstart');
      this.omniInput.input.dispatchEvent(touchstart);
      this.omniInput.input.focus();
    }
  }

  componentWillUpdate(prevProps) {
    if (this.props.show && prevProps.show !== this.props.show) {
      Mousetrap.unbind('escape');
      document.removeEventListener('mousedown', this.handleClickOutside);
    }
  }

  getSearchedCategories() {
    return ['apps', 'commands', 'groups', 'subscriptions'];
  }

  control(evt) {
    if (evt.key === 'Escape') {
      if (this.state.query.length > 0) {
        this.setState({ query: '', results: this.initialResults() });
      } else if (this.props.show) {
        this.props.api.local.setOmnibox();
      }
    };

    if (
      evt.key === 'ArrowUp' ||
      (evt.shiftKey && evt.key === 'Tab')) {
        evt.preventDefault();
      return this.setPreviousSelected();
    }

    if (evt.key === 'ArrowDown' || evt.key === 'Tab') {
      evt.preventDefault();
      this.setNextSelected();
    }

    if (evt.key === 'Enter') {
      evt.preventDefault();
      if (this.state.selected !== '') {
        this.navigate(this.state.selected);
      } else {
        this.navigate(Array.from(this.state.results.values()).flat()[0].link);
      }
    }
  }

  handleClickOutside(evt) {
    if (this.props.show && !this.omniBox.contains(evt.target)) {
      this.setState({ results: this.initialResults(), query: '' }, () => {
        this.props.api.local.setOmnibox();
      });
    }
  }

  initialResults() {
    return new Map(this.getSearchedCategories().map(category => [category, []]));
  }

  navigate(link) {
    const { props } = this;
    this.setState({ results: this.initialResults(), query: '' }, () => {
      props.api.local.setOmnibox();
      props.history.push(link);
    });
  }

  search(event) {
    const { state } = this;
    let query = event.target.value;
    const results = this.initialResults();
    let selected = state.selected;

    this.setState({ query });

    // wipe results if backspacing
    if (query.length === 0) {
      this.setState({ results: results, selected: '' });
      return;
    }

    // don't search for single characters
    if (query.length === 1) {
      return;
    }

    query = query.toLowerCase();

    this.getSearchedCategories().map((category) => {
      const categoryIndex = state.index.get(category);
      results.set(category,
        categoryIndex.filter((result) => {
          return (
            result.title.toLowerCase().includes(query) ||
            result.link.toLowerCase().includes(query) ||
            result.app.toLowerCase().includes(query) ||
            (result.host !== null ? result.host.includes(query) : false)
          );
        })
      );
    });

    const flattenedResultLinks = Array.from(results.values()).flat().map(result => result.link);
    if (!flattenedResultLinks.includes(selected)) {
      selected = flattenedResultLinks[0];
    }

    this.setState({ results, selected });
  }

  setPreviousSelected() {
    const current = this.state.selected;
    const flattenedResults = Array.from(this.state.results.values()).flat();
    const totalLength = flattenedResults.length;
    if (current !== '') {
      const currentIndex = flattenedResults.indexOf(
        ...flattenedResults.filter((e) => {
          return e.link === current;
        })
      );
      if (currentIndex > 0) {
        const nextLink = flattenedResults[currentIndex - 1].link;
        this.setState({ selected: nextLink });
      } else {
        const nextLink = flattenedResults[totalLength - 1].link;
        this.setState({ selected: nextLink });
      }
    } else {
      const nextLink = flattenedResults[totalLength - 1].link;
      this.setState({ selected: nextLink });
    }
  }

  setNextSelected() {
    const current = this.state.selected;
    const flattenedResults = Array.from(this.state.results.values()).flat();
    if (current !== '') {
      const currentIndex = flattenedResults.indexOf(
        ...flattenedResults.filter((e) => {
          return e.link === current;
        })
      );
      if (currentIndex < flattenedResults.length - 1) {
      const nextLink = flattenedResults[currentIndex + 1].link;
      this.setState({ selected: nextLink });
      } else {
      const nextLink = flattenedResults[0].link;
      this.setState({ selected: nextLink });
      }
    } else {
      const nextLink = flattenedResults[0].link;
      this.setState({ selected: nextLink });
    }
  }

  renderResults() {
    const { props, state } = this;
    return <Box
            maxHeight="400px"
            overflowY="scroll"
            overflowX="hidden"
            borderBottomLeftRadius='2'
            borderBottomRightRadius='2'
           >
      {this.getSearchedCategories()
        .map(category => Object({ category, categoryResults: state.results.get(category) }))
        .filter(category => category.categoryResults.length > 0)
        .map(({ category, categoryResults }, i) => (
          <Box key={i} width='max(50vw, 300px)' maxWidth='600px'>
            <Rule borderTopWidth="0.5px" color="washedGray" />
            <Text gray ml={2}>{category.charAt(0).toUpperCase() + category.slice(1)}</Text>
            {categoryResults.map((result, i2) => (
              <OmniboxResult
                key={i2}
                icon={result.app}
                text={result.title}
                subtext={cite(result.host)}
                link={result.link}
                navigate={() => this.navigate(result.link)}
                selected={this.state.selected}
                dark={props.dark} />
            ))}
          </Box>
        ))
      }
    </Box>;
  }

  render() {
    const { props, state } = this;
    if (!state.selected && Array.from(this.state.results.values()).flat().length) {
      this.setNextSelected();
    }
    return (
        <Box
          backgroundColor='scales.black30'
          width='100vw'
          height='100vh'
          position='absolute'
          top='0'
          right='0'
          zIndex='9'
          display={props.show ? 'block' : 'none'}>
          <Row justifyContent='center'>
            <Box
              mt='20vh'
              width='max(50vw, 300px)'
              maxWidth='600px'
              borderRadius='2'
              backgroundColor='white'
              ref={(el) => {
                this.omniBox = el;
              }}>
              <OmniboxInput
                ref={(el) => {
                  this.omniInput = el;
                }}
                control={e => this.control(e)}
                search={this.search}
                query={state.query}
              />
              {this.renderResults()}
            </Box>
          </Row>
        </Box>
    );
  }
}

export default withRouter(Omnibox);
