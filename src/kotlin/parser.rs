// https://kotlinlang.org/docs/reference/grammar.html
use super::super::choice_lazy;
use super::super::kotlin;
use super::super::Parser;
use super::data;
use super::lexer;

pub fn import_header() -> Parser<String, char, data::ImportHeader> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(&lexer::import(), &identifier()),
        &lexer::seq_left_skip_ws_nl(
            &Parser::choice(vec![
                lexer::seq2_skip_ws_nl(&lexer::dot(), &lexer::mult())
                    .map_to(data::ImportHeader2::DotMult),
                import_alias().map(data::ImportHeader2::ImportAlias),
            ])
            .optional(),
            &semi().optional(),
        ),
    )
    .map(|(identifier, import_header_op)| data::ImportHeader {
        identifier,
        import_header_op,
    })
}

pub fn import_alias() -> Parser<String, char, data::ImportAlias> {
    lexer::seq_right_skip_ws_nl(&lexer::as_(), &simple_identifier())
        .map(|simple_identifier| data::ImportAlias { simple_identifier })
}

pub fn variance_modifier() -> Parser<String, char, data::VarianceModifier> {
    choice_lazy!(
        lexer::in_().map_to(data::VarianceModifier::In),
        lexer::out().map_to(data::VarianceModifier::Out),
    )
}

pub fn annotation_use_site_target() -> Parser<String, char, data::AnnotationUseSiteTarget> {
    lexer::seq_left_skip_ws(
        &lexer::seq_right_skip_ws(
            &choice_lazy!(lexer::at_no_ws().map_to(()), lexer::at_pre_ws(),),
            &choice_lazy!(lexer::field().map_to(data::AnnotationUseSiteTarget::Field)),
        ),
        &lexer::zero_or_more_vec_skip_ws(lexer::nl).seq2(&lexer::colon()),
    )
}

pub fn unescaped_annotation() -> Parser<String, char, data::UnescapedAnnotation> {
    choice_lazy!(
        constructor_invocation().map(data::UnescapedAnnotation::ConstructorInvocation),
        user_type().map(data::UnescapedAnnotation::UserType),
    )
}

pub fn constructor_invocation() -> Parser<String, char, data::ConstructorInvocation> {
    lexer::seq2_skip_ws(&user_type(), &value_arguments()).map(|(user_type, value_arguments)| {
        data::ConstructorInvocation {
            user_type,
            value_arguments,
        }
    })
}

pub fn user_type() -> Parser<String, char, data::UserType> {
    lexer::seq2_skip_ws(
        &simple_user_type(),
        &lexer::zero_or_more_vec_skip_ws(|| {
            lexer::seq_right_skip_ws(
                &lexer::seq2_skip_ws(
                    &lexer::seq2_skip_ws(
                        &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                        &lexer::dot(),
                    ),
                    &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                ),
                &simple_user_type(),
            )
        }),
    )
    .map(
        |(x, mut xs): (data::SimpleUserType, Vec<data::SimpleUserType>)| {
            xs.insert(0, x);
            data::UserType { parts: xs }
        },
    )
}

pub fn simple_user_type() -> Parser<String, char, data::SimpleUserType> {
    lexer::seq2_skip_ws(
        &simple_identifier(),
        &lexer::seq_right_skip_ws(
            &lexer::zero_or_more_vec_skip_ws(lexer::nl),
            &type_arguments(),
        )
        .optional(),
    )
    .map(
        |(simple_identifier, type_arguments_op)| data::SimpleUserType {
            simple_identifier,
            type_arguments_op,
        },
    )
}

pub fn value_arguments() -> Parser<String, char, data::ValueArguments> {
    lexer::seq_left_skip_ws(
        &lexer::seq_right_skip_ws(
            &lexer::seq2_skip_ws(
                &lexer::lparen(),
                &lexer::zero_or_more_vec_skip_ws(lexer::nl),
            ),
            &lexer::seq_left_skip_ws(
                &lexer::seq2_skip_ws(
                    &value_argument(),
                    &lexer::zero_or_more_vec_skip_ws(|| {
                        lexer::seq_right_skip_ws(
                            &lexer::seq2_skip_ws(
                                &lexer::seq2_skip_ws(
                                    &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                                    &lexer::comma(),
                                ),
                                &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                            ),
                            &value_argument(),
                        )
                    }),
                ),
                &lexer::seq2_skip_ws(
                    &lexer::seq2_skip_ws(
                        &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                        &lexer::comma(),
                    )
                    .optional(),
                    &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                ),
            )
            .optional(),
        ),
        &lexer::rparen(),
    )
    .map(
        |x: Option<(data::ValueArgument, Vec<data::ValueArgument>)>| {
            let value_arguments;
            if let Some((x2, mut xs)) = x {
                xs.insert(0, x2);
                value_arguments = xs;
            } else {
                value_arguments = Vec::new();
            }
            data::ValueArguments { value_arguments }
        },
    )
}

pub fn value_argument() -> Parser<String, char, data::ValueArgument> {
    lexer::seq2_skip_ws(
        &lexer::seq2_skip_ws(
            &lexer::seq_left_skip_ws(
                &annotation().optional(),
                &lexer::zero_or_more_vec_skip_ws(lexer::nl),
            ),
            &lexer::seq_left_skip_ws(
                &simple_identifier(),
                &lexer::seq2_skip_ws(
                    &lexer::seq2_skip_ws(
                        &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                        &lexer::assignment(),
                    ),
                    &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                ),
            )
            .optional(),
        ),
        &lexer::seq_right_skip_ws(
            &lexer::seq2_skip_ws(
                &lexer::mult().optional(),
                &lexer::zero_or_more_vec_skip_ws(lexer::nl),
            ),
            &Parser::lazy(expression),
        ),
    )
    .map(
        |((annotation_op, simple_identifier_op), expression)| data::ValueArgument {
            annotation_op,
            simple_identifier_op,
            expression,
        },
    )
}

pub fn annotation() -> Parser<String, char, data::Annotation> {
    lexer::seq_left_skip_ws(
        &choice_lazy!(
            single_annotation().map(data::Annotation::SingleAnnotation),
            multi_annotation().map(data::Annotation::MutliAnnotation),
        ),
        &lexer::zero_or_more_vec_skip_ws(lexer::nl),
    )
}

pub fn single_annotation() -> Parser<String, char, data::SingleAnnotation> {
    lexer::seq2_skip_ws(
        &choice_lazy!(
            lexer::seq_left_skip_ws(
                &annotation_use_site_target().map(Some),
                &lexer::zero_or_more_vec_skip_ws(lexer::nl),
            ),
            lexer::at_no_ws().map_to(None),
            lexer::at_pre_ws().map_to(None)
        ),
        &unescaped_annotation(),
    )
    .map(
        |(annotation_use_site_target_op, unescaped_annotation)| data::SingleAnnotation {
            annotation_use_site_target_op,
            unescaped_annotation,
        },
    )
}

pub fn multi_annotation() -> Parser<String, char, data::MultiAnnotation> {
    lexer::seq2_skip_ws(
        &choice_lazy!(
            lexer::seq_left_skip_ws(
                &annotation_use_site_target().map(Some),
                &lexer::zero_or_more_vec_skip_ws(lexer::nl),
            ),
            lexer::at_no_ws().map_to(None),
            lexer::at_pre_ws().map_to(None)
        ),
        &lexer::seq_left_skip_ws(
            &lexer::seq_right_skip_ws(
                &lexer::lsquare(),
                &lexer::one_or_more_vec_skip_ws(unescaped_annotation),
            ),
            &lexer::rsquare(),
        ),
    )
    .map(
        |(annotation_use_site_target_op, unescaped_annotations)| data::MultiAnnotation {
            annotation_use_site_target_op,
            unescaped_annotations,
        },
    )
}

pub fn type_arguments() -> Parser<String, char, data::TypeArguments> {
    lexer::seq2_skip_ws(
        &lexer::seq_right_skip_ws(
            &lexer::seq2_skip_ws(
                &lexer::langle(),
                &lexer::zero_or_more_vec_skip_ws(lexer::nl),
            ),
            &type_projection(),
        ),
        &lexer::seq_left_skip_ws(
            &lexer::zero_or_more_vec_skip_ws(|| {
                lexer::seq_right_skip_ws(
                    &lexer::seq2_skip_ws(
                        &lexer::seq2_skip_ws(
                            &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                            &lexer::comma(),
                        ),
                        &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                    ),
                    &type_projection(),
                )
            }),
            &lexer::seq2_skip_ws(
                &lexer::seq2_skip_ws(
                    &lexer::seq2_skip_ws(
                        &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                        &lexer::comma(),
                    ),
                    &lexer::zero_or_more_vec_skip_ws(lexer::nl),
                ),
                &lexer::range(),
            ),
        ),
    )
    .map(
        |(x, mut xs): (data::TypeProjection, Vec<data::TypeProjection>)| {
            xs.insert(0, x);
            data::TypeArguments {
                type_projections: xs,
            }
        },
    )
}

pub fn type_projection() -> Parser<String, char, data::TypeProjection> {
    choice_lazy!(
        lexer::seq2_skip_ws(&type_projection_modifiers().optional(), &type_()).map(
            |(type_projection_modifiers_op, type_)| {
                data::TypeProjection::TypeProjectionModifiersAndType(
                    data::TypeProjectionModifiersAndType {
                        type_projection_modifiers_op,
                        type_,
                    },
                )
            }
        ),
        lexer::mult().map_to(data::TypeProjection::Mult)
    )
}

pub fn type_projection_modifiers() -> Parser<String, char, data::TypeProjectionModifiers> {
    lexer::one_or_more_vec_skip_ws(type_projection_modifier).map(|type_projection_modifiers| {
        data::TypeProjectionModifiers {
            type_projection_modifiers,
        }
    })
}

pub fn type_projection_modifier() -> Parser<String, char, data::TypeProjectionModifier> {
    choice_lazy!(
        lexer::seq_left_skip_ws(
            &variance_modifier(),
            &lexer::zero_or_more_vec_skip_ws(lexer::nl)
        )
        .map(data::TypeProjectionModifier::VarianceModifier),
        annotation().map(data::TypeProjectionModifier::Annotation)
    )
}

pub fn type_() -> Parser<String, char, data::Type> {
    lexer::seq2_skip_ws(
        &type_modifiers().optional(),
        &choice_lazy!(
            parenthesized_type().map(data::Type2::ParenthesizedType),
            nullable_type().map(data::Type2::NullableType),
            type_reference().map(data::Type2::TypeReference),
            function_type().map(data::Type2::FunctionType),
        ),
    )
    .map(|(type_modifiers_op, type_)| data::Type {
        type_modifiers_op,
        type_,
    })
}

pub fn type_modifiers() -> Parser<String, char, data::TypeModifiers> {
    lexer::one_or_more_vec_skip_ws(type_modifier)
        .map(|type_modifiers| data::TypeModifiers { type_modifiers })
}

pub fn type_modifier() -> Parser<String, char, data::TypeModifier> {
    choice_lazy!(
        annotation().map(data::TypeModifier::Annotation),
        lexer::seq2_skip_ws(
            &lexer::suspend(),
            &lexer::zero_or_more_vec_skip_ws(lexer::nl)
        )
        .map_to(data::TypeModifier::Suspend)
    )
}

pub fn parenthesized_type() -> Parser<String, char, data::ParenthesizedType> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(&lexer::lparen(), &type_()),
        &lexer::rparen(),
    )
    .map(|type_| data::ParenthesizedType {
        type_: Box::new(type_),
    })
}

pub fn nullable_type() -> Parser<String, char, data::NullableType> {
    lexer::seq_left_skip_ws(
        &lexer::seq_left_skip_ws(
            &choice_lazy!(
                type_reference().map(data::NullableType::TypeReference),
                parenthesized_type().map(data::NullableType::ParenthesizedType),
            ),
            &lexer::zero_or_more_vec_skip_ws(lexer::nl),
        ),
        &lexer::one_or_more_vec_skip_ws(quest),
    )
}

pub fn quest() -> Parser<String, char, ()> {
    choice_lazy!(lexer::quest_no_ws().map_to(()), lexer::quest_ws(),)
}

pub fn type_reference() -> Parser<String, char, data::TypeReference> {
    choice_lazy!(
        user_type().map(data::TypeReference::UserType),
        lexer::dynamic().map_to(data::TypeReference::Dynamic),
    )
}

pub fn function_type() -> Parser<String, char, data::FunctionType> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq_left_skip_ws_nl(&receiver_type(), &lexer::dot()).optional(),
        &lexer::seq2_skip_ws_nl(
            &function_type_parameters(),
            &lexer::seq_right_skip_ws_nl(&lexer::arrow(), &type_()),
        ),
    )
    .map(
        |(receiver_type_op, (function_type_parameters, type_))| data::FunctionType {
            receiver_type_op,
            function_type_parameters,
            type_: Box::new(type_),
        },
    )
}

pub fn receiver_type() -> Parser<String, char, data::ReceiverType> {
    lexer::seq2_skip_ws(
        &type_modifiers().optional(),
        &choice_lazy!(
            parenthesized_type().map(data::ReceiverType2::ParenthesizedType),
            nullable_type().map(data::ReceiverType2::NullableType),
            type_reference().map(data::ReceiverType2::TypeReference),
        ),
    )
    .map(|(type_modifiers_op, receiver_type)| data::ReceiverType {
        type_modifiers_op,
        receiver_type,
    })
}

pub fn function_type_parameters() -> Parser<String, char, data::FunctionTypeParameters> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(
            &lexer::lparen(),
            &lexer::zero_or_more_sep_by_skip_ws_nl(
                &choice_lazy!(
                    parameter().map(data::ParameterOrType::Parameter),
                    type_().map(data::ParameterOrType::Type),
                ),
                &lexer::comma(),
            ),
        ),
        &lexer::rparen(),
    )
    .map(|parameters_or_types| data::FunctionTypeParameters {
        parameters_or_types,
    })
}

pub fn parameter() -> Parser<String, char, data::Parameter> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq_left_skip_ws_nl(&simple_identifier(), &lexer::colon()),
        &type_(),
    )
    .map(|(simple_identifier, type_)| data::Parameter {
        simple_identifier,
        type_,
    })
}

pub fn expression() -> Parser<String, char, data::Expression> {
    disjunction().map(|disjunction| data::Expression { disjunction })
}

pub fn disjunction() -> Parser<String, char, data::Disjunction> {
    lexer::one_or_more_sep_by_skip_ws_nl(&conjunction(), &lexer::disj())
        .map(|conjunctions| data::Disjunction { conjunctions })
}

pub fn conjunction() -> Parser<String, char, data::Conjunction> {
    lexer::one_or_more_sep_by_skip_ws_nl(&equality(), &lexer::conj())
        .map(|equalities| data::Conjunction { equalities })
}

pub fn equality() -> Parser<String, char, data::Equality> {
    lexer::seq2_skip_ws_nl(
        &comparison(),
        &lexer::zero_or_more_vec_skip_ws_nl(|| {
            lexer::seq2_skip_ws_nl(&equality_operator(), &comparison())
        }),
    )
    .map(|(first, rest)| data::Equality(first, rest))
}

pub fn equality_operator() -> Parser<String, char, data::EqualityOperator> {
    choice_lazy!(
        lexer::excl_eq().map_to(data::EqualityOperator::ExclEq),
        lexer::excl_eqeq().map_to(data::EqualityOperator::ExclEqEq),
        lexer::eqeq().map_to(data::EqualityOperator::EqEq),
        lexer::eqeqeq().map_to(data::EqualityOperator::EqEqEq),
    )
}

pub fn comparison() -> Parser<String, char, data::Comparison> {
    lexer::seq2_skip_ws_nl(
        &generic_call_like_comparison(),
        &lexer::zero_or_more_vec_skip_ws_nl(|| {
            lexer::seq2_skip_ws_nl(&comparison_operator(), &generic_call_like_comparison())
        }),
    )
    .map(|(first, rest)| data::Comparison(first, rest))
}

pub fn comparison_operator() -> Parser<String, char, data::ComparisonOperator> {
    choice_lazy!(
        lexer::langle().map_to(data::ComparisonOperator::LT),
        lexer::rangle().map_to(data::ComparisonOperator::GT),
        lexer::le().map_to(data::ComparisonOperator::LE),
        lexer::ge().map_to(data::ComparisonOperator::GE),
    )
}

pub fn generic_call_like_comparison() -> Parser<String, char, data::GenericCallLikeComparison> {
    lexer::seq2_skip_ws_nl(
        &infix_operation(),
        &lexer::zero_or_more_vec_skip_ws_nl(call_suffix),
    )
    .map(
        |(infix_operation, call_suffixes)| data::GenericCallLikeComparison {
            infix_operation,
            call_suffixes,
        },
    )
}

pub fn infix_operation() -> Parser<String, char, data::InfixOperation> {
    lexer::seq2_skip_ws_nl(
        &elvis_expression(),
        &lexer::zero_or_more_vec_skip_ws_nl(|| {
            choice_lazy!(
                lexer::seq2_skip_ws_nl(&in_operator(), &elvis_expression()).map(
                    |(in_operator, elvis_expression)| data::InfixOperation2::_1(
                        in_operator,
                        elvis_expression
                    )
                ),
                lexer::seq2_skip_ws_nl(&is_operator(), &type_(),)
                    .map(|(is_operator, type_)| data::InfixOperation2::_2(is_operator, type_)),
            ) as Parser<String, char, data::InfixOperation2>
        }),
    )
    .map(
        |(elvis_expression, infix_operations)| data::InfixOperation {
            elvis_expression,
            infix_operations,
        },
    )
}

pub fn elvis_expression() -> Parser<String, char, data::ElvisExpression> {
    lexer::seq2_skip_ws_nl(
        &infix_function_call(),
        &lexer::zero_or_more_vec_skip_ws_nl(|| {
            lexer::seq_right_skip_ws_nl(&elvis(), &infix_function_call())
        }),
    )
    .map(|(x, mut xs)| {
        xs.insert(0, x);
        data::ElvisExpression {
            infix_function_calls: xs,
        }
    })
}

pub fn elvis() -> Parser<String, char, ()> {
    lexer::seq2_skip_ws_nl(&lexer::quest_no_ws(), &lexer::colon()).map_to(())
}

pub fn infix_function_call() -> Parser<String, char, data::InfixFunctionCall> {
    lexer::seq2_skip_ws_nl(
        &range_expression(),
        &lexer::zero_or_more_vec_skip_ws_nl(|| {
            lexer::seq2_skip_ws_nl(&simple_identifier(), &range_expression())
        }),
    )
    .map(|(first, rest)| data::InfixFunctionCall(first, rest))
}

pub fn range_expression() -> Parser<String, char, data::RangeExpression> {
    lexer::zero_or_more_sep_by_skip_ws_nl(&additive_expression(), &lexer::range())
        .map(|additive_expressions| data::RangeExpression(additive_expressions))
}

pub fn additive_expression() -> Parser<String, char, data::AdditiveExpression> {
    lexer::seq2_skip_ws_nl(
        &multiplicative_expression(),
        &lexer::zero_or_more_vec_skip_ws_nl(|| {
            lexer::seq2_skip_ws_nl(&additive_operator(), &multiplicative_expression())
        }),
    )
    .map(|(first, rest)| data::AdditiveExpression(first, rest))
}

pub fn additive_operator() -> Parser<String, char, data::AdditiveOperator> {
    Parser::choice(vec![
        lexer::add().map_to(data::AdditiveOperator::Add),
        lexer::sub().map_to(data::AdditiveOperator::Sub),
    ])
}

pub fn multiplicative_expression() -> Parser<String, char, data::MultiplicativeExpression> {
    lexer::seq2_skip_ws_nl(
        &as_expression(),
        &lexer::zero_or_more_vec_skip_ws_nl(|| {
            lexer::seq2_skip_ws_nl(&multiplicative_operator(), &as_expression())
        }),
    )
    .map(|(first, rest)| data::MultiplicativeExpression(first, rest))
}

pub fn multiplicative_operator() -> Parser<String, char, data::MultiplicativeOperator> {
    Parser::choice(vec![
        lexer::mult().map_to(data::MultiplicativeOperator::Mult),
        lexer::div().map_to(data::MultiplicativeOperator::Div),
        lexer::mod_().map_to(data::MultiplicativeOperator::Mod),
    ])
}

pub fn as_expression() -> Parser<String, char, data::AsExpression> {
    lexer::seq2_skip_ws_nl(
        &prefix_unary_expression(),
        &lexer::zero_or_more_vec_skip_ws_nl(|| lexer::seq2_skip_ws_nl(&as_operator(), &type_())),
    )
    .map(|(first, rest)| data::AsExpression(first, rest))
}

pub fn as_operator() -> Parser<String, char, data::AsOperator> {
    Parser::choice(vec![
        lexer::as_().map_to(data::AsOperator::As),
        lexer::as_safe().map_to(data::AsOperator::AsSafe),
    ])
}

pub fn prefix_unary_expression() -> Parser<String, char, data::PrefixUnaryExpression> {
    lexer::seq2_skip_ws_nl(
        &lexer::zero_or_more_vec_skip_ws_nl(unary_prefix),
        &postfix_unary_expression(),
    )
    .map(|(rest, last)| data::PrefixUnaryExpression(rest, last))
}

pub fn unary_prefix() -> Parser<String, char, data::UnaryPrefix> {
    Parser::choice(vec![
        annotation().map(data::UnaryPrefix::Annotation),
        label().map(data::UnaryPrefix::Label),
        prefix_unary_operator().map(data::UnaryPrefix::PrefixUnaryOperator),
    ])
}

pub fn prefix_unary_operator() -> Parser<String, char, data::PrefixUnaryOperator> {
    Parser::choice(vec![
        lexer::incr().map_to(data::PrefixUnaryOperator::Incr),
        lexer::decr().map_to(data::PrefixUnaryOperator::Decr),
        lexer::sub().map_to(data::PrefixUnaryOperator::Sub),
        lexer::add().map_to(data::PrefixUnaryOperator::Add),
        excl().map_to(data::PrefixUnaryOperator::Excl),
    ])
}

pub fn excl() -> Parser<String, char, ()> {
    Parser::choice(vec![
        lexer::excl_no_ws().map_to(()),
        lexer::excl_ws().map_to(()),
    ])
}

pub fn postfix_unary_expression() -> Parser<String, char, data::PostfixUnaryExpression> {
    lexer::seq2_skip_ws_nl(
        &primary_expression(),
        &lexer::zero_or_more_vec_skip_ws_nl(postfix_unary_suffix),
    )
    .map(|(first, rest)| data::PostfixUnaryExpression(first, rest))
}

pub fn primary_expression() -> Parser<String, char, data::PrimaryExpression> {
    Parser::choice(vec![
        parenthesized_expression().map(data::PrimaryExpression::ParenthesizedExpression),
        literal_constant().map(data::PrimaryExpression::LiteralConstant),
        string_literal().map(data::PrimaryExpression::StringLiteral),
        callable_reference().map(data::PrimaryExpression::CallableReference),
        function_literal().map(data::PrimaryExpression::FunctionLiteral),
        object_literal().map(data::PrimaryExpression::ObjectLiteral),
        collection_literal().map(data::PrimaryExpression::CollectionLiteral),
        this_expression().map(data::PrimaryExpression::ThisExpression),
        super_expression().map(data::PrimaryExpression::SuperExpression),
        if_expression().map(data::PrimaryExpression::IfExpression),
        when_expression().map(data::PrimaryExpression::WhenExpression),
        try_expression().map(data::PrimaryExpression::TryExpression),
        jump_expression().map(data::PrimaryExpression::JumpExpression),
    ])
}

pub fn parenthesized_expression() -> Parser<String, char, data::ParenthesizedExpression> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(&lexer::lparen(), &Parser::lazy(expression)),
        &lexer::rparen(),
    )
    .map(|expression| data::ParenthesizedExpression {
        expression: Box::new(expression),
    })
}

pub fn literal_constant() -> Parser<String, char, data::LiteralConstant> {
    Parser::choice(vec![
        lexer::boolean_literal().map(data::LiteralConstant::BooleanLiteral),
        lexer::integer_literal().map(data::LiteralConstant::IntegerLiteral),
        lexer::hex_literal().map(data::LiteralConstant::HexLiteral),
        lexer::character_literal().map(data::LiteralConstant::CharacterLiteral),
        lexer::real_literal().map(data::LiteralConstant::RealLiteral),
        lexer::null_literal().map_to(data::LiteralConstant::NullLiteral),
        lexer::long_literal().map(data::LiteralConstant::LongLiteral),
        lexer::unsigned_literal().map(data::LiteralConstant::UnsignedLiteral),
    ])
}

pub fn string_literal() -> Parser<String, char, data::StringLiteral> {
    Parser::choice(vec![
        line_string_literal().map(data::StringLiteral::LineStringLiteral),
        multi_line_string_literal().map(data::StringLiteral::MultiLineStringLiteral),
    ])
}

pub fn line_string_literal() -> Parser<String, char, data::LineStringLiteral> {
    lexer::quote()
        .seq_right(
            &Parser::choice(vec![
                line_string_content().map(data::LineStringContentOrExpression::LineStringContent),
                line_string_expression()
                    .map(data::LineStringContentOrExpression::LineStringExpression),
            ])
            .zero_or_more_vec(),
        )
        .seq_left(&lexer::quote())
        .map(data::LineStringLiteral)
}

pub fn line_string_content() -> Parser<String, char, data::LineStringContent> {
    Parser::choice(vec![
        lexer::line_str_text().map(data::LineStringContent::LineStrText),
        lexer::line_str_escaped_char().map(data::LineStringContent::LineStrEscapedChar),
        lexer::line_str_ref().map(data::LineStringContent::LineStrRef),
    ])
}

pub fn line_string_expression() -> Parser<String, char, data::LineStringExpression> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(&lexer::line_str_expr_start(), &Parser::lazy(expression)),
        &lexer::rcurl(),
    )
    .map(|expression| data::LineStringExpression {
        expression: Box::new(expression),
    })
}

pub fn multi_line_string_literal() -> Parser<String, char, data::MultiLineStringLiteral> {
    lexer::triple_quote()
        .seq_right(&Parser::choice(vec![
            multi_line_string_content().map(data::MultiLineStringLiteral::MultiLineStringContent),
            multi_line_string_expression()
                .map(data::MultiLineStringLiteral::MultiLineStringExpression),
        ]))
        .seq_left(&lexer::triple_quote())
}

pub fn multi_line_string_content() -> Parser<String, char, data::MultiLineStringContent> {
    Parser::choice(vec![
        lexer::multi_line_str_text().map(data::MultiLineStringContent::MultiLineStrText),
        lexer::multi_line_string_quote()
            .map(|count| data::MultiLineStringContent::MultiLineStringQuote { count }),
        lexer::multi_line_str_ref().map(data::MultiLineStringContent::MultiLineStrRef),
    ])
}

pub fn multi_line_string_expression() -> Parser<String, char, data::MultiLineStringExpression> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(
            &lexer::multi_line_str_expr_start(),
            &Parser::lazy(expression),
        ),
        &lexer::rcurl(),
    )
    .map(|expression| data::MultiLineStringExpression {
        expression: Box::new(expression),
    })
}

pub fn callable_reference() -> Parser<String, char, data::CallableReference> {
    lexer::seq2_skip_ws_nl(
        &receiver_type().optional(),
        &lexer::seq_right_skip_ws_nl(
            &lexer::coloncolon(),
            &Parser::choice(vec![
                simple_identifier().map(data::CallableReference2::SimpleIdentifier),
                lexer::class().map_to(data::CallableReference2::Class),
            ]),
        ),
    )
    .map(
        |(receiver_type_op, callable_reference)| data::CallableReference {
            receiver_type_op,
            callable_reference,
        },
    )
}

pub fn function_literal() -> Parser<String, char, data::FunctionLiteral> {
    Parser::choice(vec![
        lambda_literal().map(data::FunctionLiteral::LambdaLiteral),
        anonymous_function().map(data::FunctionLiteral::AnonymousFunction),
    ])
}

pub fn object_literal() -> Parser<String, char, data::ObjectLiteral> {
    lexer::seq_right_skip_ws_nl(
        &lexer::object(),
        &lexer::seq2_skip_ws_nl(
            &lexer::seq_right_skip_ws_nl(&lexer::colon(), &delegation_specifiers()).optional(),
            &class_body().optional(),
        ),
    )
    .map(
        |(delegation_specifiers_op, class_body_op)| data::ObjectLiteral {
            delegation_specifiers_op,
            class_body_op,
        },
    )
}

pub fn delegation_specifiers() -> Parser<String, char, data::DelegationSpecifiers> {
    lexer::one_or_more_sep_by_skip_ws_nl(&annotated_delegation_specifier(), &lexer::comma())
        .map(data::DelegationSpecifiers)
}

pub fn annotated_delegation_specifier() -> Parser<String, char, data::AnnotatedDelegationSpecifier>
{
    lexer::seq2_skip_ws_nl(
        &lexer::zero_or_more_vec_skip_ws_nl(annotation),
        &delegation_specifier(),
    )
    .map(
        |(annotations, delegation_specifier)| data::AnnotatedDelegationSpecifier {
            annotations,
            delegation_specifier,
        },
    )
}

pub fn delegation_specifier() -> Parser<String, char, data::DelegationSpecifier> {
    Parser::choice(vec![
        constructor_invocation().map(data::DelegationSpecifier::Constructorinvocation),
        explicit_delegation().map(data::DelegationSpecifier::ExplicitDelegation),
        user_type().map(data::DelegationSpecifier::UserType),
        function_type().map(data::DelegationSpecifier::FunctionType),
    ])
}

pub fn explicit_delegation() -> Parser<String, char, data::ExplicitDelegation> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq_left_skip_ws_nl(
            &Parser::choice(vec![
                user_type().map(data::ExplicitDelegation2::UserType),
                function_type().map(data::ExplicitDelegation2::FunctionType),
            ]),
            &lexer::by(),
        ),
        &Parser::lazy(expression),
    )
    .map(
        |(explicit_delegation, expression)| data::ExplicitDelegation {
            explicit_delegation,
            expression: Box::new(expression),
        },
    )
}

pub fn class_body() -> Parser<String, char, data::ClassBody> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(&lexer::lcurl(), &class_member_declarations()),
        &lexer::rcurl(),
    )
    .map(|class_member_declarations| data::ClassBody {
        class_member_declarations,
    })
}

pub fn class_member_declarations() -> Parser<String, char, data::ClassMemberDeclarations> {
    lexer::zero_or_more_vec_skip_ws_nl(|| {
        lexer::seq_left_skip_ws_nl(&class_member_declaration(), &semis().optional())
    })
    .map(data::ClassMemberDeclarations)
}

pub fn class_member_declaration() -> Parser<String, char, data::ClassMemberDeclaration> {
    Parser::choice(vec![
        declaration().map(data::ClassMemberDeclaration::Declaration),
        companion_object().map(data::ClassMemberDeclaration::CompanionObject),
        anonymous_initializer().map(data::ClassMemberDeclaration::AnonymousInitializer),
        secondary_constructor().map(data::ClassMemberDeclaration::SecondaryConstructor),
    ])
}

pub fn declaration() -> Parser<String, char, data::Declaration> {
    Parser::choice(vec![
        class_declaration().map(data::Declaration::ClassDeclaration),
        object_declaration().map(data::Declaration::ObjectDeclaration),
        function_declaration().map(data::Declaration::FunctionDeclaration),
        property_declaration().map(data::Declaration::PropertyDeclaration),
        type_alias().map(data::Declaration::TypeAlias),
    ])
}

pub fn class_declaration() -> Parser<String, char, data::ClassDeclaration> {
    lexer::seq2_skip_ws_nl(
        &modifiers().optional(),
        &lexer::seq2_skip_ws_nl(
            &Parser::choice(vec![
                lexer::class().map_to(data::ClassDeclaration2::Class),
                lexer::seq_left_skip_ws_nl(
                    &lexer::fun().optional().map(|x| x.is_some()),
                    &lexer::interface(),
                )
                .map(|fun| data::ClassDeclaration2::Interface { fun }),
            ]),
            &lexer::seq2_skip_ws_nl(
                &simple_identifier(),
                &lexer::seq2_skip_ws_nl(
                    &type_parameters().optional(),
                    &lexer::seq2_skip_ws_nl(
                        &primary_constructor().optional(),
                        &lexer::seq2_skip_ws_nl(
                            &lexer::seq_right_skip_ws_nl(&lexer::colon(), &delegation_specifiers())
                                .optional(),
                            &lexer::seq2_skip_ws_nl(
                                &type_constraints().optional(),
                                &Parser::choice(vec![
                                    Parser::lazy(class_body)
                                        .map(data::ClassDeclaration3::ClassBody),
                                    Parser::lazy(enum_class_body)
                                        .map(data::ClassDeclaration3::EnumClassBody),
                                ])
                                .optional(),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    .map(
        |(
            modifiers_op,
            (
                class_declaration,
                (
                    simple_identifier,
                    (
                        type_parameters_op,
                        (
                            primary_constructor_op,
                            (
                                delegation_specifiers_op,
                                (type_constraints_op, class_declaration2_op),
                            ),
                        ),
                    ),
                ),
            ),
        )| {
            data::ClassDeclaration {
                modifiers_op,
                class_declaration,
                simple_identifier,
                type_parameters_op,
                primary_constructor_op,
                delegation_specifiers_op,
                type_constraints_op,
                class_declaration2_op,
            }
        },
    )
}

pub fn modifiers() -> Parser<String, char, data::Modifiers> {
    lexer::zero_or_more_vec_skip_ws_nl(|| {
        Parser::choice(vec![
            annotation().map(data::Modifiers2::Annotation),
            modifier().map(data::Modifiers2::Modifier),
        ])
    })
    .map(data::Modifiers)
}

pub fn modifier() -> Parser<String, char, data::Modifier> {
    Parser::choice(vec![
        class_modifier().map(data::Modifier::ClassModifier),
        member_modifier().map(data::Modifier::MemberModifier),
        visibility_modifier().map(data::Modifier::VisibilityModifier),
        function_modifier().map(data::Modifier::FunctionModifier),
        property_modifier().map(data::Modifier::PropertyModifier),
        inheritance_modifier().map(data::Modifier::InheritanceModifier),
        parameter_modifier().map(data::Modifier::ParameterModifier),
        platform_modifier().map(data::Modifier::PlatformModifier),
    ])
}

pub fn class_modifier() -> Parser<String, char, data::ClassModifier> {
    Parser::choice(vec![
        lexer::enum_().map_to(data::ClassModifier::Enum),
        lexer::sealed().map_to(data::ClassModifier::Sealed),
        lexer::annotation().map_to(data::ClassModifier::Annotation),
        lexer::data().map_to(data::ClassModifier::Data),
        lexer::inner().map_to(data::ClassModifier::Inner),
        lexer::value().map_to(data::ClassModifier::Value),
    ])
}

pub fn member_modifier() -> Parser<String, char, data::MemberModifier> {
    Parser::choice(vec![
        lexer::override_().map_to(data::MemberModifier::Override),
        lexer::lateinit().map_to(data::MemberModifier::LateInit),
    ])
}

pub fn visibility_modifier() -> Parser<String, char, data::VisibilityModifier> {
    Parser::choice(vec![
        lexer::public().map_to(data::VisibilityModifier::Public),
        lexer::private().map_to(data::VisibilityModifier::Private),
        lexer::internal().map_to(data::VisibilityModifier::Internal),
        lexer::protected().map_to(data::VisibilityModifier::Protected),
    ])
}

pub fn function_modifier() -> Parser<String, char, data::FunctionModifier> {
    Parser::choice(vec![
        lexer::tailrec().map_to(data::FunctionModifier::TailRec),
        lexer::operator().map_to(data::FunctionModifier::Operator),
        lexer::infix().map_to(data::FunctionModifier::Infix),
        lexer::inline().map_to(data::FunctionModifier::Inline),
        lexer::external().map_to(data::FunctionModifier::External),
        lexer::suspend().map_to(data::FunctionModifier::Suspend),
    ])
}

pub fn property_modifier() -> Parser<String, char, data::PropertyModifier> {
    lexer::const_().map_to(data::PropertyModifier::Const)
}

pub fn inheritance_modifier() -> Parser<String, char, data::InheritanceModifier> {
    Parser::choice(vec![
        lexer::abstract_().map_to(data::InheritanceModifier::Abstract),
        lexer::final_().map_to(data::InheritanceModifier::Final),
        lexer::open().map_to(data::InheritanceModifier::Open),
    ])
}

pub fn platform_modifier() -> Parser<String, char, data::PlatformModifier> {
    Parser::choice(vec![
        lexer::expect().map_to(data::PlatformModifier::Expect),
        lexer::actual().map_to(data::PlatformModifier::Actual),
    ])
}

pub fn type_parameters() -> Parser<String, char, data::TypeParameters> {
    lexer::seq_right_skip_ws_nl(
        &lexer::langle(),
        &lexer::seq_left_skip_ws_nl(
            &lexer::one_or_more_sep_by_skip_ws_nl(&type_parameter(), &lexer::comma()),
            &lexer::rangle(),
        ),
    )
    .map(data::TypeParameters)
}

pub fn type_parameter() -> Parser<String, char, data::TypeParameter> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq2_skip_ws_nl(&type_parameter_modifiers().optional(), &simple_identifier()),
        &lexer::seq_right_skip_ws_nl(&lexer::colon(), &Parser::lazy(type_)).optional(),
    )
    .map(
        |((type_parameter_modifiers_op, simple_identifier), type_op)| data::TypeParameter {
            type_parameter_modifiers_op,
            simple_identifier,
            type_op: type_op.map(Box::new),
        },
    )
}

pub fn type_parameter_modifiers() -> Parser<String, char, data::TypeParameterModifiers> {
    lexer::zero_or_more_vec_skip_ws_nl(type_parameter_modifier).map(data::TypeParameterModifiers)
}

pub fn type_parameter_modifier() -> Parser<String, char, data::TypeParameterModifier> {
    Parser::choice(vec![
        reification_modifier().map(data::TypeParameterModifier::ReificationModifier),
        variance_modifier().map(data::TypeParameterModifier::VarianceModifier),
        annotation().map(data::TypeParameterModifier::Annotation),
    ])
}

pub fn reification_modifier() -> Parser<String, char, data::ReificationModifier> {
    lexer::reified().map_to(data::ReificationModifier::Reified)
}

pub fn enum_class_body() -> Parser<String, char, data::EnumClassBody> {
    lexer::seq_right_skip_ws_nl(
        &lexer::lcurl(),
        &lexer::seq_left_skip_ws_nl(
            &lexer::seq2_skip_ws_nl(
                &enum_entries().optional(),
                &lexer::seq_right_skip_ws_nl(&lexer::semicolon(), &class_member_declarations())
                    .optional(),
            ),
            &lexer::rcurl(),
        ),
    )
    .map(
        |(enum_entries_op, class_member_declarations_op)| data::EnumClassBody {
            enum_entries_op,
            class_member_declarations_op,
        },
    )
}

pub fn enum_entries() -> Parser<String, char, data::EnumEntries> {
    lexer::seq_left_skip_ws_nl(
        &lexer::one_or_more_sep_by_skip_ws_nl(&enum_entry(), &lexer::comma()),
        &lexer::comma().optional(),
    )
    .map(data::EnumEntries)
}

pub fn enum_entry() -> Parser<String, char, data::EnumEntry> {
    lexer::seq2_skip_ws_nl(
        &modifiers().optional(),
        &lexer::seq2_skip_ws_nl(
            &simple_identifier(),
            &lexer::seq2_skip_ws_nl(&value_arguments().optional(), &class_body().optional()),
        ),
    )
    .map(
        |(modifiers_op, (simple_identifier, (value_arguments_op, class_body_op)))| {
            data::EnumEntry {
                modifiers_op,
                simple_identifier,
                value_arguments_op,
                class_body_op,
            }
        },
    )
}

pub fn primary_constructor() -> Parser<String, char, data::PrimaryConstructor> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq_left_skip_ws_nl(&modifiers(), &lexer::constructor()).optional(),
        &class_parameters(),
    )
    .map(
        |(modifiers_op, class_parameters)| data::PrimaryConstructor {
            modifiers_op,
            class_parameters,
        },
    )
}

pub fn class_parameters() -> Parser<String, char, data::ClassParameters> {
    lexer::seq_right_skip_ws_nl(
        &lexer::lparen(),
        &lexer::seq_left_skip_ws_nl(
            &lexer::seq_left_skip_ws_nl(
                &lexer::zero_or_more_sep_by_skip_ws_nl(&class_parameter(), &lexer::comma()),
                &lexer::comma().optional(),
            ),
            &lexer::rparen(),
        ),
    )
    .map(data::ClassParameters)
}

pub fn class_parameter() -> Parser<String, char, data::ClassParameter> {
    lexer::seq2_skip_ws_nl(
        &modifiers().optional(),
        &lexer::seq2_skip_ws_nl(
            &Parser::choice(vec![
                lexer::val().map_to(data::ValOrVar::Val),
                lexer::var().map_to(data::ValOrVar::Var),
            ])
            .optional(),
            &lexer::seq2_skip_ws_nl(
                &simple_identifier(),
                &lexer::seq_right_skip_ws_nl(
                    &lexer::colon(),
                    &lexer::seq2_skip_ws_nl(
                        &Parser::lazy(type_),
                        &lexer::seq_right_skip_ws_nl(
                            &lexer::assignment(),
                            &Parser::lazy(expression),
                        )
                        .optional(),
                    ),
                ),
            ),
        ),
    )
    .map(
        |(modifiers_op, (val_or_var_op, (simple_identifier, (type_, expression_op))))| {
            data::ClassParameter {
                modifiers_op,
                val_or_var_op,
                simple_identifier,
                type_: type_,
                expression_op,
            }
        },
    )
}

pub fn object_declaration() -> Parser<String, char, data::ObjectDeclaration> {
    lexer::seq2_skip_ws_nl(
        &modifiers().optional(),
        &lexer::seq_right_skip_ws_nl(
            &lexer::object(),
            &lexer::seq2_skip_ws_nl(
                &simple_identifier(),
                &lexer::seq2_skip_ws_nl(
                    &lexer::seq_right_skip_ws_nl(
                        &lexer::colon(),
                        &delegation_specifiers()
                    )
                    .optional(),
                    &class_body().optional(),
                )
            )
        )
    )
    .map(|(modifiers_op, (simple_identifier, (delegation_specifiers_op, class_body_op)))| {
        data::ObjectDeclaration {
            modifiers_op,
            simple_identifier,
            delegation_specifiers_op,
            class_body_op,
        }
    })
}

pub fn function_declaration() -> Parser<String, char, data::FunctionDeclaration> {
    Parser::unimplemented()
}

pub fn property_declaration() -> Parser<String, char, data::PropertyDeclaration> {
    Parser::unimplemented()
}

pub fn type_alias() -> Parser<String, char, data::TypeAlias> {
    Parser::unimplemented()
}

pub fn companion_object() -> Parser<String, char, data::CompanionObject> {
    Parser::unimplemented()
}

pub fn anonymous_initializer() -> Parser<String, char, data::AnonymousInitializer> {
    Parser::unimplemented()
}

pub fn secondary_constructor() -> Parser<String, char, data::SecondaryConstructor> {
    Parser::unimplemented()
}

pub fn collection_literal() -> Parser<String, char, data::CollectionLiteral> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(
            &lexer::lsquare(),
            &lexer::zero_or_more_sep_by_skip_ws_nl(&Parser::lazy(expression), &lexer::comma()),
        ),
        &lexer::rsquare(),
    )
    .map(data::CollectionLiteral)
}

pub fn this_expression() -> Parser<String, char, data::ThisExpression> {
    Parser::unimplemented()
}

pub fn super_expression() -> Parser<String, char, data::SuperExpression> {
    Parser::unimplemented()
}

pub fn if_expression() -> Parser<String, char, data::IfExpression> {
    Parser::unimplemented()
}

pub fn when_expression() -> Parser<String, char, data::WhenExpression> {
    Parser::unimplemented()
}

pub fn try_expression() -> Parser<String, char, data::TryExpression> {
    Parser::unimplemented()
}

pub fn jump_expression() -> Parser<String, char, data::JumpExpression> {
    Parser::unimplemented()
}

pub fn postfix_unary_suffix() -> Parser<String, char, data::PostfixUnarySuffix> {
    Parser::choice(vec![
        postfix_unary_operator().map(data::PostfixUnarySuffix::PostfixUnaryOperator),
        type_arguments().map(data::PostfixUnarySuffix::TypeArguments),
        call_suffix().map(data::PostfixUnarySuffix::CallSuffix),
        indexing_suffix().map(data::PostfixUnarySuffix::IndexingSuffix),
        navigation_suffix().map(data::PostfixUnarySuffix::NavigationSuffix),
    ])
}

pub fn postfix_unary_operator() -> Parser<String, char, data::PostfixUnaryOperator> {
    Parser::choice(vec![
        lexer::incr().map_to(data::PostfixUnaryOperator::Incr),
        lexer::decr().map_to(data::PostfixUnaryOperator::Decr),
        lexer::seq2_skip_ws_nl(&lexer::excl_no_ws(), &excl())
            .map_to(data::PostfixUnaryOperator::Excl),
    ])
}

pub fn indexing_suffix() -> Parser<String, char, data::IndexingSuffix> {
    lexer::seq_right_skip_ws_nl(
        &lexer::lsquare(),
        &lexer::seq_left_skip_ws_nl(
            &lexer::one_or_more_sep_by_skip_ws_nl(&Parser::lazy(expression), &lexer::comma()),
            &lexer::seq2_skip_ws_nl(&lexer::comma().optional(), &lexer::rsquare()),
        ),
    )
    .map(data::IndexingSuffix)
}

pub fn navigation_suffix() -> Parser<String, char, data::NavigationSuffix> {
    lexer::seq2_skip_ws_nl(
        &member_access_operator(),
        &Parser::choice(vec![
            simple_identifier().map(data::NavigationSuffix2::SimpleIdentifier),
            parenthesized_expression().map(data::NavigationSuffix2::ParenthesizedExpression),
            lexer::class().map_to(data::NavigationSuffix2::Class),
        ]),
    )
    .map(
        |(member_access_operator, navigation_suffix)| data::NavigationSuffix {
            member_access_operator,
            navigation_suffix,
        },
    )
}

pub fn member_access_operator() -> Parser<String, char, data::MemberAccessOperator> {
    Parser::choice(vec![
        lexer::dot().map_to(data::MemberAccessOperator::Dot),
        safe_nav().map_to(data::MemberAccessOperator::SafeNav),
        lexer::coloncolon().map_to(data::MemberAccessOperator::ColonColon),
    ])
}

pub fn safe_nav() -> Parser<String, char, ()> {
    lexer::seq2_skip_ws_nl(&lexer::quest_no_ws(), &lexer::dot()).map_to(())
}

pub fn in_operator() -> Parser<String, char, data::InOperator> {
    Parser::choice(vec![
        lexer::in_().map_to(data::InOperator::In),
        lexer::not_in().map_to(data::InOperator::NotIn),
    ])
}

pub fn is_operator() -> Parser<String, char, data::IsOperator> {
    Parser::choice(vec![
        lexer::is().map_to(data::IsOperator::Is),
        lexer::not_is().map_to(data::IsOperator::NotIs),
    ])
}

pub fn call_suffix() -> Parser<String, char, data::CallSuffix> {
    lexer::seq2_skip_ws_nl(
        &type_arguments().optional(),
        &choice_lazy!(
            lexer::seq2_skip_ws_nl(&value_arguments().optional(), &annotated_lambda()).map(
                |(value_arguments_op, annotated_lambda)| data::CallSuffix2::_1(
                    value_arguments_op,
                    annotated_lambda
                )
            ),
            value_arguments().map(data::CallSuffix2::_2)
        ),
    )
    .map(|(type_arguments_op, call_suffix)| data::CallSuffix {
        type_arguments_op,
        call_suffix,
    })
}

pub fn annotated_lambda() -> Parser<String, char, data::AnnotatedLambda> {
    lexer::seq2_skip_ws_nl(
        &lexer::zero_or_more_vec_skip_ws_nl(annotation),
        &lexer::seq2_skip_ws_nl(&label().optional(), &lambda_literal()),
    )
    .map(
        |(annotations, (label_op, lambda_literal))| data::AnnotatedLambda {
            annotations,
            label_op,
            lambda_literal,
        },
    )
}

pub fn label() -> Parser<String, char, data::Label> {
    lexer::seq_left_skip_ws_nl(
        &simple_identifier(),
        &Parser::choice(vec![lexer::at_no_ws().map_to(()), lexer::at_post_ws()]),
    )
    .map(|simple_identifier| data::Label { simple_identifier })
}

pub fn lambda_literal() -> Parser<String, char, data::LambdaLiteral> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(
            &lexer::lcurl(),
            &lexer::seq_left_skip_ws_nl(&lambda_parameters(), &lexer::arrow()).optional(),
        ),
        &lexer::seq_left_skip_ws_nl(&statements(), &lexer::rcurl()),
    )
    .map(|(lambda_parameters_op, statements)| data::LambdaLiteral {
        lambda_parameters_op,
        statements,
    })
}

pub fn lambda_parameters() -> Parser<String, char, data::LambdaParameters> {
    lexer::seq_left_skip_ws_nl(
        &lexer::zero_or_more_sep_by_skip_ws_nl(&lambda_parameter(), &lexer::comma()),
        &lexer::comma().optional(),
    )
    .map(|lambda_parameters| data::LambdaParameters { lambda_parameters })
}

pub fn lambda_parameter() -> Parser<String, char, data::LambdaParameter> {
    Parser::choice(vec![
        variable_declaration().map(data::LambdaParameter::VariableDeclaration),
        lexer::seq2_skip_ws_nl(
            &lexer::seq_left_skip_ws_nl(&multi_variable_declaration(), &lexer::colon()),
            &type_().optional(),
        )
        .map(|(multi_variable_declaration, type_op)| {
            data::LambdaParameter::MultiVariableDeclaration(multi_variable_declaration, type_op)
        }),
    ])
}

pub fn anonymous_function() -> Parser<String, char, data::AnonymousFunction> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq2_skip_ws_nl(
            &lexer::seq2_skip_ws_nl(
                &lexer::seq2_skip_ws_nl(
                    &lexer::seq_right_skip_ws_nl(
                        &lexer::fun(),
                        &lexer::seq_left_skip_ws_nl(&type_(), &lexer::dot()).optional(),
                    ),
                    &parameters_with_optional_type(),
                ),
                &lexer::seq_right_skip_ws_nl(&lexer::colon(), &type_()).optional(),
            ),
            &type_constraints().optional(),
        ),
        &function_body().optional(),
    )
    .map(
        |(
            (((this_type_op, parameters_with_optional_type), return_type_op), type_constraints_op),
            function_body_op,
        )| {
            data::AnonymousFunction {
                this_type_op,
                parameters_with_optional_type,
                return_type_op,
                type_constraints_op,
                function_body_op,
            }
        },
    )
}

pub fn parameters_with_optional_type() -> Parser<String, char, data::ParametersWithOptionalType> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq_right_skip_ws_nl(
            &lexer::lparen(),
            &lexer::zero_or_more_sep_by_skip_ws_nl(
                &function_value_parameter_with_optional_type(),
                &lexer::comma(),
            ),
        ),
        &lexer::seq2_skip_ws_nl(&lexer::comma().optional(), &lexer::rparen()),
    )
    .map(data::ParametersWithOptionalType)
}

pub fn function_value_parameter_with_optional_type(
) -> Parser<String, char, data::FunctionValueParameterWithOptionalType> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq2_skip_ws_nl(
            &parameter_modifiers().optional(),
            &parameter_with_optional_type(),
        ),
        &lexer::seq_right_skip_ws_nl(&lexer::assignment(), &Parser::lazy(expression)).optional(),
    )
    .map(
        |((parameter_modifiers_op, parameter_with_optional_type), expression_op)| {
            data::FunctionValueParameterWithOptionalType {
                parameter_modifiers_op,
                parameter_with_optional_type,
                expression_op: expression_op.map(Box::new),
            }
        },
    )
}

pub fn parameter_modifiers() -> Parser<String, char, data::ParameterModifiers> {
    lexer::zero_or_more_vec_skip_ws_nl(|| {
        Parser::choice(vec![
            annotation().map(data::ParameterModifiers2::Annotation),
            parameter_modifier().map(data::ParameterModifiers2::ParameterModifier),
        ])
    })
    .map(data::ParameterModifiers)
}

pub fn parameter_modifier() -> Parser<String, char, data::ParameterModifier> {
    Parser::choice(vec![
        lexer::vararg().map_to(data::ParameterModifier::VarArg),
        lexer::noinline().map_to(data::ParameterModifier::NoInline),
        lexer::crossinline().map_to(data::ParameterModifier::CrossInline),
    ])
}

pub fn parameter_with_optional_type() -> Parser<String, char, data::ParameterWithOptionalType> {
    lexer::seq2_skip_ws_nl(
        &simple_identifier(),
        &lexer::seq_right_skip_ws_nl(&lexer::colon(), &type_()).optional(),
    )
    .map(
        |(simple_identifier, type_op)| data::ParameterWithOptionalType {
            simple_identifier,
            type_op,
        },
    )
}

pub fn type_constraints() -> Parser<String, char, data::TypeConstraints> {
    lexer::seq_right_skip_ws_nl(
        &lexer::where_(),
        &lexer::zero_or_more_sep_by_skip_ws_nl(&type_constraint(), &lexer::comma()),
    )
    .map(data::TypeConstraints)
}

pub fn type_constraint() -> Parser<String, char, data::TypeConstraint> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq_left_skip_ws_nl(
            &lexer::seq2_skip_ws_nl(
                &lexer::zero_or_more_vec_skip_ws_nl(annotation),
                &simple_identifier(),
            ),
            &lexer::colon(),
        ),
        &type_(),
    )
    .map(
        |((annotations, simple_identifier), type_)| data::TypeConstraint {
            annotations,
            simple_identifier,
            type_,
        },
    )
}

pub fn function_body() -> Parser<String, char, data::FunctionBody> {
    Parser::choice(vec![
        block().map(data::FunctionBody::Block),
        lexer::seq_right_skip_ws_nl(
            &lexer::assignment(),
            &Parser::lazy(expression).map(data::FunctionBody::Expression),
        ),
    ])
}

pub fn block() -> Parser<String, char, data::Block> {
    lexer::seq_right_skip_ws_nl(
        &lexer::lcurl(),
        &lexer::seq_left_skip_ws_nl(&statements(), &lexer::rcurl()),
    )
    .map(data::Block)
}

pub fn variable_declaration() -> Parser<String, char, data::VariableDeclaration> {
    lexer::seq2_skip_ws_nl(
        &lexer::seq2_skip_ws_nl(
            &lexer::zero_or_more_vec_skip_ws_nl(annotation),
            &simple_identifier(),
        ),
        &lexer::seq_right_skip_ws_nl(&lexer::colon(), &type_()).optional(),
    )
    .map(
        |((annotations, simple_identifier), type_op)| data::VariableDeclaration {
            annotations,
            simple_identifier,
            type_op,
        },
    )
}

pub fn multi_variable_declaration() -> Parser<String, char, data::MultiVariableDeclaration> {
    lexer::seq_left_skip_ws_nl(
        &lexer::seq2_skip_ws_nl(
            &lexer::seq_right_skip_ws_nl(&lexer::lparen(), &variable_declaration()),
            &lexer::zero_or_more_vec_skip_ws_nl(|| {
                lexer::seq_right_skip_ws_nl(&lexer::comma(), &variable_declaration())
            }),
        ),
        &lexer::seq2_skip_ws_nl(&lexer::comma().optional(), &lexer::rparen()),
    )
    .map(|(x, mut xs)| {
        xs.insert(0, x);
        data::MultiVariableDeclaration {
            variable_declarations: xs,
        }
    })
}

pub fn statements() -> Parser<String, char, data::Statements> {
    lexer::seq_left_skip_ws_nl(
        &lexer::zero_or_more_sep_by_skip_ws_nl(&statement(), &semis()),
        &semis().optional(),
    )
    .map(data::Statements)
}

pub fn semi() -> Parser<String, char, ()> {
    Parser::choice(vec![
        lexer::seq2_skip_ws_nl(
            &Parser::choice(vec![lexer::semicolon().map_to(()), lexer::nl()]),
            &lexer::zero_or_more_vec_skip_ws(lexer::nl),
        )
        .map_to(()),
        Parser::eof(),
    ])
}

pub fn semis() -> Parser<String, char, ()> {
    Parser::choice(vec![
        lexer::one_or_more_vec_skip_ws(|| {
            Parser::choice(vec![lexer::semicolon().map_to(()), lexer::nl()])
        })
        .map_to(()),
        Parser::eof(),
    ])
}

pub fn statement() -> Parser<String, char, data::Statement> {
    lexer::seq2_skip_ws_nl(
        &lexer::zero_or_more_vec_skip_ws_nl(|| {
            Parser::choice(vec![
                label().map(data::LabelOrAnnotation::Label),
                annotation().map(data::LabelOrAnnotation::Annotation),
            ])
        }),
        &Parser::choice(vec![
            declaration().map(data::Statement2::Declaration),
            assignment().map(data::Statement2::Assignment),
            loop_statement().map(data::Statement2::LoopStatement),
            Parser::lazy(expression).map(data::Statement2::Expression),
        ]),
    )
    .map(|(labels_and_annotations, statement)| data::Statement {
        labels_and_annotations,
        statement,
    })
}

pub fn assignment() -> Parser<String, char, data::Assignment> {
    Parser::unimplemented()
}

pub fn loop_statement() -> Parser<String, char, data::LoopStatement> {
    Parser::unimplemented()
}

pub fn simple_identifier() -> Parser<String, char, String> {
    choice_lazy!(
        kotlin::lexer::identifier().map_to(()),
        kotlin::lexer::abstract_(),
        kotlin::lexer::annotation(),
        kotlin::lexer::by(),
        kotlin::lexer::catch(),
        kotlin::lexer::companion(),
        kotlin::lexer::constructor(),
        kotlin::lexer::crossinline(),
        kotlin::lexer::data(),
        kotlin::lexer::dynamic(),
        kotlin::lexer::enum_(),
        kotlin::lexer::external(),
        kotlin::lexer::final_(),
        kotlin::lexer::finally(),
        kotlin::lexer::get(),
        kotlin::lexer::import(),
        kotlin::lexer::infix(),
        kotlin::lexer::init(),
        kotlin::lexer::inline(),
        kotlin::lexer::inner(),
        kotlin::lexer::internal(),
        kotlin::lexer::lateinit(),
        kotlin::lexer::noinline(),
        kotlin::lexer::open(),
        kotlin::lexer::operator(),
        kotlin::lexer::out(),
        kotlin::lexer::override_(),
        kotlin::lexer::private(),
        kotlin::lexer::protected(),
        kotlin::lexer::public(),
        kotlin::lexer::reified(),
        kotlin::lexer::sealed(),
        kotlin::lexer::tailrec(),
        kotlin::lexer::set(),
        kotlin::lexer::vararg(),
        kotlin::lexer::where_(),
        kotlin::lexer::field(),
        kotlin::lexer::property(),
        kotlin::lexer::receiver(),
        kotlin::lexer::param(),
        kotlin::lexer::setparam(),
        kotlin::lexer::delegate(),
        kotlin::lexer::file(),
        kotlin::lexer::expect(),
        kotlin::lexer::actual(),
        kotlin::lexer::const_(),
        kotlin::lexer::suspend(),
        kotlin::lexer::value(),
    )
    .return_string()
}

pub fn identifier() -> Parser<String, char, data::Identifier> {
    lexer::seq2_skip_ws(
        &simple_identifier(),
        &lexer::zero_or_more_vec_skip_ws(|| {
            lexer::seq_right_skip_ws(
                &lexer::seq2_skip_ws(
                    &lexer::zero_or_more_vec_skip_ws(|| lexer::nl()),
                    &lexer::dot(),
                ),
                &simple_identifier(),
            )
        }),
    )
    .map(|(x, mut xs): (String, Vec<String>)| {
        xs.insert(0, x);
        super::data::Identifier { parts: xs }
    })
}
