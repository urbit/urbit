import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { Box, Row, Rule, Text } from '@tlon/indigo-react';
import index from '../lib/omnibox';
import Mousetrap from 'mousetrap';
import OmniboxInput from './OmniboxInput';
import OmniboxResult from './OmniboxResult';

import { cite } from '../lib/util';

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
    return new Map([
      ['commands', []],
      ['subscriptions', []],
      ['groups', []],
      ['apps', []]
    ]);
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
    const query = event.target.value;
    const results = this.initialResults();

    this.setState({ query: query });

    // wipe results if backspacing
    if (query.length === 0) {
      this.setState({ results: results, selected: '' });
      return;
    }

    // don't search for single characters
    if (query.length === 1) {
      return;
    }

    ['commands', 'subscriptions', 'groups', 'apps'].map((category) => {
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

    this.setState({ results: results });
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

  render() {
    const { props, state } = this;
    const categoryResult = [];

    const renderResults = <Box maxHeight="400px" overflowY="scroll" overflowX="hidden">
      {categoryResult}
    </Box>;

      ['commands', 'subscriptions', 'groups', 'apps'].map((category, i) => {
      const categoryResults = state.results.get(category);
      if (categoryResults.length > 0) {
        const each = categoryResults.map((result, i) => {
          return <OmniboxResult
            key={i}
            icon={result.app}
            text={result.title}
            subtext={cite(result.host)}
            link={result.link}
            navigate={() => this.navigate(result.link)}
            selected={this.state.selected}
            dark={props.dark}
                 />;
        });
         categoryResult.push(<Box key={i} width='max(50vw, 300px)' maxWidth='600px'>
        <Rule borderTopWidth="0.5px" color="washedGray" />
        <Text gray ml={2}>{category.charAt(0).toUpperCase() + category.slice(1)}</Text>
          {each}
        </Box>);
      }
    });

    return (
        <Box
          backgroundColor='lightGray'
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
                ref={(el) => { this.omniInput = el; }}
                control={e => this.control(e)}
                search={this.search}
                query={state.query}
              />
              {renderResults}
            </Box>
          </Row>
        </Box>
    );
  }
}

export default withRouter(Omnibox);
