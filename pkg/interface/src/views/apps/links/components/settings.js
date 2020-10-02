import React, { Component } from 'react';

import { Link } from 'react-router-dom';

import { LoadingScreen } from './loading';
import { Spinner } from '~/views/components/Spinner';
import { TabBar } from '~/views/components/chat-link-tabbar';
import SidebarSwitcher from '~/views/components/SidebarSwitch';

import { MetadataSettings } from '~/views/components/metadata/settings';

import { Box, Text, Button, Col, Row } from '@tlon/indigo-react';

export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isLoading: false,
      awaiting: false,
      type: 'Editing'
    };

    this.renderDelete = this.renderDelete.bind(this);
    this.changeLoading = this.changeLoading.bind(this);
  }

  componentDidUpdate() {
    const { props, state } = this;

    if (Boolean(state.isLoading) && !props.resource) {
      this.setState({
        isLoading: false
      }, () => {
        props.history.push('/~link');
      });
    }
  }

  changeLoading(isLoading, awaiting, type, closure) {
    this.setState({
      isLoading,
      awaiting,
      type
    }, closure);
  }

  removeCollection() {
    const { props } = this;

    this.setState({
      isLoading: true,
      awaiting: true,
      type: 'Removing'
    });

    props.api.graph.leaveGraph(
      `~${props.match.params.ship}`,
      props.match.params.name
    );
  }

  deleteCollection() {
    const { props } = this;

    this.setState({
      isLoading: true,
      awaiting: true,
      type: 'Deleting'
    });

    props.api.graph.deleteGraph(props.match.params.name);
  }

  renderRemove() {
    const { props } = this;

    if (props.amOwner) {
      return null;
    } else {
      return (
        <Box width='100%' mt='3'>
          <Text display='block' mt='3' fontSize='1' mb='1'>Remove Collection</Text>
          <Text display='block' fontSize='0' gray mb='4'>
            Remove this collection from your collection list
          </Text>
          <Button onClick={this.removeCollection.bind(this)}>
            Remove collection
          </Button>
        </Box>
      );
    }
  }

  renderDelete() {
    const { props } = this;

    if (!props.amOwner) {
      return null;
    } else {
      return (
        <Box width='100%' mt='3'>
          <Text fontSize='1' mt='3' display='block' mb='1'>Delete collection</Text>
          <Text fontSize='0' gray display='block' mb='4'>
            Delete this collection, for you and all group members
          </Text>
          <Button primary onClick={this.deleteCollection.bind(this)} destructive mb='4'>
            Delete collection
          </Button>
        </Box>
      );
    }
  }

  render() {
    const { props, state } = this;
    const title = props.resource.metadata.title || props.resourcePath;

    if (
      (!props.hasGraph || !props.resource.metadata.color)
      && props.graphResource
    ) {
      return <LoadingScreen />;
    } else if (!props.graphResource) {
      props.history.push('/~link');
      return <Box />;
    }

    return (
      <Col height='100%' width='100' overflowX='hidden'>
        <Box width='100%' display={['block', 'none']} pt='4' pb='6' pl='3' fontSize='1' height='1rem'>
          <Link to="/~link">{'⟵ All Collections'}</Link>
        </Box>
        <Row
          pl='12px'
          pt='2'
          borderBottom='1px solid'
          borderColor='washedGray'
          flexShrink='0'
          overflowX={['scroll', 'auto']}
          height='48px'
        >
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
            api={this.props.api}
          />
          <Link className="dib f9 fw4 pt2 gray2 lh-solid"
                to={`/~link/${props.resourcePath}`}>
            <Text
              display='inline-block'
              fontSize='0'
              verticalAlign='top'
              width='max-content'>
              {title}
            </Text>
          </Link>
          <TabBar
            location={props.location}
            popout={props.popout}
            popoutHref={`/~link/popout/${props.resourcePath}/settings`}
            settings={`/~link/${props.resourcePath}/settings`}
          />
          </Row>
        <Box width='100' pl='3' mt='3'>
          <Text display='block' fontSize='1' pb='2'>Collection Settings</Text>
          {this.renderRemove()}
          {this.renderDelete()}
          <MetadataSettings
            isOwner={props.amOwner}
            changeLoading={this.changeLoading}
            api={props.api}
            association={props.resource}
            resource="collection"
            app="graph"
            module="link"
          />
          <Spinner
            awaiting={this.state.awaiting}
            classes="absolute right-1 bottom-1 pa2 ba b--black b--gray0-d white-d"
            text={this.state.type}
          />
        </Box>
      </Col>
    );
  }
}
