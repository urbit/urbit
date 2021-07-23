import React, { Component } from 'react';
import { Text, Box, Col, Button, BaseAnchor } from '@tlon/indigo-react';
import { RouteComponentProps, withRouter } from 'react-router-dom';
import styled from 'styled-components';

type ErrorProps = RouteComponentProps & {
  code?: number | string,
  description?: string,
  error?: Error
};

const Summary = styled.summary`
  color: ${ p => p.theme.colors.black };
`;

const Details = styled.details``;

class ErrorComponent extends Component<ErrorProps> {
  render () {
    const { code, error, history, description } = this.props;
    let title = '';
    if (error) {
      title = error.message;
    } else if (description) {
      title = description;
    }
    let body = '';
    if (error) {
      body =`\`\`\`%0A${error.stack?.replaceAll('\n', '%0A')}%0A\`\`\``;
    }
    return (
      <Col alignItems="center" justifyContent="center" height="100%" p="4" backgroundColor="white" maxHeight="100%">
        <Box mb={4}>
          <Text fontSize={3}>
           {code ? code : 'Error'}
         </Text>
       </Box>
       { description && (<Box mb={4}><Text>{description}</Text></Box>) }
       {error && (
         <Box mb={4} style={{ maxWidth: '100%' }}>
           <Box mb={2}>
             <Text mono>&ldquo;{error.message}&rdquo;</Text>
           </Box>
           <Details>
              <Summary>Stack trace</Summary>
              <Text
                mono
                p={1}
                borderRadius={1}
                display='block'
                overflow='scroll'
                maxHeight='50vh'
                backgroundColor='washedGray'
                style={{ whiteSpace: 'pre', wordWrap: 'break-word' }}
              >{error.stack}</Text>
           </Details>
          </Box>
       )}
          <Text mb={4} textAlign="center">If this is unexpected, email <code>support@tlon.io</code> or <BaseAnchor borderBottom={1} color='black' href={`https://github.com/urbit/landscape/issues/new?assignees=&labels=bug&title=${title}&body=${body}`} target="_blank">submit an issue</BaseAnchor>.</Text>
          {history.length > 1
            ? <Button primary onClick={() => history.go(-1) }>Go back</Button>
            : <Button primary onClick={() => history.push('/') }>Go home</Button>
          }
        </Col>
      );
  }
}

export default withRouter(ErrorComponent);
