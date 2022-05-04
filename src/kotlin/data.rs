#[derive(Clone, Debug, PartialEq)]
pub struct KotlinFile {
    pub shebang_line: Option<ShebangLine>,
    pub file_annotations: Vec<FileAnnotation>,
    pub package_header: PackageHeader,
    pub import_list: ImportList,
    pub top_level_objects: Vec<TopLevelObject>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Script {
    pub shebang_line: Option<ShebangLine>,
    pub file_annotations: Vec<FileAnnotation>,
    pub package_header: PackageHeader,
    pub import_list: ImportList,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShebangLine {
    pub content: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FileAnnotation {
    pub unescaped_annotations: Vec<UnescapedAnnotation>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PackageHeader {
    pub identifier: Identifier,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportList {
    pub import_headers: Vec<ImportHeader>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportHeader {
    pub identifier: Identifier,
    pub import_header_op: Option<ImportHeader2>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportHeader2 {
    DotMult,
    ImportAlias(ImportAlias),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportAlias {
    pub simple_identifier: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TopLevelObject;

#[derive(Clone, Debug, PartialEq)]
pub struct Statements(pub Vec<Statement>);

#[derive(Clone, Debug, PartialEq)]
pub struct Statement {
    pub labels_and_annotations: Vec<LabelOrAnnotation>,
    pub statement: Statement2,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement2 {
    Declaration(Declaration),
    Assignment(Assignment),
    LoopStatement(LoopStatement),
    Expression(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LabelOrAnnotation {
    Label(Label),
    Annotation(Annotation),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Label {
    pub simple_identifier: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Annotation {
    SingleAnnotation(SingleAnnotation),
    MutliAnnotation(MultiAnnotation),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SingleAnnotation {
    pub annotation_use_site_target_op: Option<AnnotationUseSiteTarget>,
    pub unescaped_annotation: UnescapedAnnotation,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnescapedAnnotation {
    ConstructorInvocation(ConstructorInvocation),
    UserType(UserType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstructorInvocation {
    pub user_type: UserType,
    pub value_arguments: ValueArguments,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValueArguments {
    pub value_arguments: Vec<ValueArgument>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValueArgument {
    pub annotation_op: Option<Annotation>,
    pub simple_identifier_op: Option<String>,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UserType {
    pub parts: Vec<SimpleUserType>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SimpleUserType {
    pub simple_identifier: String,
    pub type_arguments_op: Option<TypeArguments>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeArguments {
    pub type_projections: Vec<TypeProjection>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeProjection {
    TypeProjectionModifiersAndType(TypeProjectionModifiersAndType),
    Mult,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeProjectionModifiersAndType {
    pub type_projection_modifiers_op: Option<TypeProjectionModifiers>,
    pub type_: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeProjectionModifiers {
    pub type_projection_modifiers: Vec<TypeProjectionModifier>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MultiAnnotation {
    pub annotation_use_site_target_op: Option<AnnotationUseSiteTarget>,
    pub unescaped_annotations: Vec<UnescapedAnnotation>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    ClassDeclaration(ClassDeclaration),
    ObjectDeclaration(ObjectDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    PropertyDeclaration(PropertyDeclaration),
    TypeAlias(TypeAlias),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassDeclaration {
    pub modifiers_op: Option<Modifiers>,
    pub class_declaration: ClassDeclaration2,
    pub simple_identifier: String,
    pub type_parameters_op: Option<TypeParameters>,
    pub primary_constructor_op: Option<PrimaryConstructor>,
    pub delegation_specifiers_op: Option<DelegationSpecifiers>,
    pub type_constraints_op: Option<TypeConstraints>,
    pub class_declaration2_op: Option<ClassDeclaration3>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeParameters(pub Vec<TypeParameter>);

#[derive(Clone, Debug, PartialEq)]
pub struct TypeParameter {
    pub type_parameter_modifiers_op: Option<TypeParameterModifiers>,
    pub simple_identifier: String,
    pub type_op: Option<Box<Type>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeParameterModifiers(pub Vec<TypeParameterModifier>);

#[derive(Clone, Debug, PartialEq)]
pub enum TypeParameterModifier {
    ReificationModifier(ReificationModifier),
    VarianceModifier(VarianceModifier),
    Annotation(Annotation),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ReificationModifier {
    Reified,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrimaryConstructor {
    pub modifiers_op: Option<Modifiers>,
    pub class_parameters: ClassParameters,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassParameters(pub Vec<ClassParameter>);

#[derive(Clone, Debug, PartialEq)]
pub struct ClassParameter {
    pub modifiers_op: Option<Modifiers>,
    pub val_or_var_op: Option<ValOrVar>,
    pub simple_identifier: String,
    pub type_: Type,
    pub expression_op: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValOrVar {
    Val,
    Var,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassDeclaration2 {
    Class,
    Interface { fun: bool },
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassDeclaration3 {
    ClassBody(ClassBody),
    EnumClassBody(EnumClassBody),
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumClassBody {
    pub enum_entries_op: Option<EnumEntries>,
    pub class_member_declarations_op: Option<ClassMemberDeclarations>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumEntries(pub Vec<EnumEntry>);

#[derive(Clone, Debug, PartialEq)]
pub struct EnumEntry {
    pub modifiers_op: Option<Modifiers>,
    pub simple_identifier: String,
    pub value_arguments_op: Option<ValueArguments>,
    pub class_body_op: Option<ClassBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectDeclaration {
    pub modifiers_op: Option<Modifiers>,
    pub simple_identifier: String,
    pub delegation_specifiers_op: Option<DelegationSpecifiers>,
    pub class_body_op: Option<ClassBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub modifiers_op: Option<Modifiers>,
    pub type_parameters_op: Option<TypeParameters>,
    pub receiver_type_op: Option<ReceiverType>,
    pub simple_identifier: String,
    pub function_value_parameters: FunctionValueParameters,
    pub type_op: Option<Type>,
    pub type_constraints_op: Option<TypeConstraints>,
    pub function_body_op: Option<FunctionBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionValueParameters(pub Vec<FunctionValueParameter>);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionValueParameter {
    pub parameter_modifiers_op: Option<ParameterModifiers>,
    pub parameter: Parameter,
    pub expression_op: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PropertyDeclaration;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeAlias;

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment;

#[derive(Clone, Debug, PartialEq)]
pub enum LoopStatement {
    ForStatement(ForStatement),
    WhileStatement(WhileStatement),
    DoWhileStatement(DoWhileStatement),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForStatement {
    pub annotations: Vec<Annotation>,
    pub variable_or_multi_variable_declaration: VariableOrMultiVariableDeclaration,
    pub expression: Expression,
    pub control_structure_body: Option<ControlStructureBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VariableOrMultiVariableDeclaration {
    VariableDeclaration(VariableDeclaration),
    MultiVariableDeclaration(MultiVariableDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStatement {
    pub expression: Expression,
    pub control_structure_body: Option<ControlStructureBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DoWhileStatement {
    pub control_structure_body: Option<ControlStructureBody>,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration {
    pub annotations: Vec<Annotation>,
    pub simple_identifier: String,
    pub type_op: Option<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MultiVariableDeclaration {
    pub variable_declarations: Vec<VariableDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ControlStructureBody;

#[derive(Clone, Debug, PartialEq)]
pub struct Expression {
    pub disjunction: Disjunction,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Disjunction {
    pub conjunctions: Vec<Conjunction>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Conjunction {
    pub equalities: Vec<Equality>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Equality(pub Comparison, pub Vec<(EqualityOperator, Comparison)>);

#[derive(Clone, Debug, PartialEq)]
pub enum EqualityOperator {
    ExclEq,
    ExclEqEq,
    EqEq,
    EqEqEq,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Comparison(
    pub GenericCallLikeComparison,
    pub Vec<(ComparisonOperator, GenericCallLikeComparison)>,
);

#[derive(Clone, Debug, PartialEq)]
pub enum ComparisonOperator {
    LT,
    GT,
    LE,
    GE,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericCallLikeComparison {
    pub infix_operation: InfixOperation,
    pub call_suffixes: Vec<CallSuffix>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InfixOperation {
    pub elvis_expression: ElvisExpression,
    pub infix_operations: Vec<InfixOperation2>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InfixOperation2 {
    _1(InOperator, ElvisExpression),
    _2(IsOperator, Type),
}

#[derive(Clone, Debug, PartialEq)]
pub enum InOperator {
    In,
    NotIn,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IsOperator {
    Is,
    NotIs,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElvisExpression {
    pub infix_function_calls: Vec<InfixFunctionCall>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallSuffix {
    pub type_arguments_op: Option<TypeArguments>,
    pub call_suffix: CallSuffix2,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CallSuffix2 {
    _1(Option<ValueArguments>, AnnotatedLambda),
    _2(ValueArguments),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnnotatedLambda {
    pub annotations: Vec<Annotation>,
    pub label_op: Option<Label>,
    pub lambda_literal: LambdaLiteral,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaLiteral {
    pub lambda_parameters_op: Option<LambdaParameters>,
    pub statements: Statements,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaParameters {
    pub lambda_parameters: Vec<LambdaParameter>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LambdaParameter {
    VariableDeclaration(VariableDeclaration),
    MultiVariableDeclaration(MultiVariableDeclaration, Option<Type>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub type_modifiers_op: Option<TypeModifiers>,
    pub type_: Type2,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeModifiers {
    pub type_modifiers: Vec<TypeModifier>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeModifier {
    Annotation(Annotation),
    Suspend,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type2 {
    ParenthesizedType(ParenthesizedType),
    NullableType(NullableType),
    TypeReference(TypeReference),
    FunctionType(FunctionType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParenthesizedType {
    pub type_: Box<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NullableType {
    TypeReference(TypeReference),
    ParenthesizedType(ParenthesizedType),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeReference {
    UserType(UserType),
    Dynamic,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub receiver_type_op: Option<ReceiverType>,
    pub function_type_parameters: FunctionTypeParameters,
    pub type_: Box<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReceiverType {
    pub type_modifiers_op: Option<TypeModifiers>,
    pub receiver_type: ReceiverType2,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ReceiverType2 {
    ParenthesizedType(ParenthesizedType),
    NullableType(NullableType),
    TypeReference(TypeReference),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionTypeParameters {
    pub parameters_or_types: Vec<ParameterOrType>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterOrType {
    Parameter(Parameter),
    Type(Type),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub simple_identifier: String,
    pub type_: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InfixFunctionCall(pub RangeExpression, pub Vec<(String, RangeExpression)>);

#[derive(Clone, Debug, PartialEq)]
pub struct RangeExpression(pub Vec<AdditiveExpression>);

#[derive(Clone, Debug, PartialEq)]
pub struct AdditiveExpression(
    pub MultiplicativeExpression,
    pub Vec<(AdditiveOperator, MultiplicativeExpression)>,
);

#[derive(Clone, Debug, PartialEq)]
pub enum AdditiveOperator {
    Add,
    Sub,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MultiplicativeExpression(
    pub AsExpression,
    pub Vec<(MultiplicativeOperator, AsExpression)>,
);

#[derive(Clone, Debug, PartialEq)]
pub enum MultiplicativeOperator {
    Mult,
    Div,
    Mod,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AsExpression(pub PrefixUnaryExpression, pub Vec<(AsOperator, Type)>);

#[derive(Clone, Debug, PartialEq)]
pub enum AsOperator {
    As,
    AsSafe,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixUnaryExpression(pub Vec<UnaryPrefix>, pub PostfixUnaryExpression);

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryPrefix {
    Annotation(Annotation),
    Label(Label),
    PrefixUnaryOperator(PrefixUnaryOperator),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrefixUnaryOperator {
    Incr,
    Decr,
    Sub,
    Add,
    Excl,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PostfixUnaryExpression(pub PrimaryExpression, pub Vec<PostfixUnarySuffix>);

#[derive(Clone, Debug, PartialEq)]
pub enum PostfixUnarySuffix {
    PostfixUnaryOperator(PostfixUnaryOperator),
    TypeArguments(TypeArguments),
    CallSuffix(CallSuffix),
    IndexingSuffix(IndexingSuffix),
    NavigationSuffix(NavigationSuffix),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PostfixUnaryOperator {
    Incr,
    Decr,
    Excl,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexingSuffix(pub Vec<Expression>);

#[derive(Clone, Debug, PartialEq)]
pub struct NavigationSuffix {
    pub member_access_operator: MemberAccessOperator,
    pub navigation_suffix: NavigationSuffix2,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NavigationSuffix2 {
    SimpleIdentifier(String),
    ParenthesizedExpression(ParenthesizedExpression),
    Class,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberAccessOperator {
    Dot,
    SafeNav,
    ColonColon,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimaryExpression {
    ParenthesizedExpression(ParenthesizedExpression),
    SimpleIdentifier(String),
    LiteralConstant(LiteralConstant),
    StringLiteral(StringLiteral),
    CallableReference(CallableReference),
    FunctionLiteral(FunctionLiteral),
    ObjectLiteral(ObjectLiteral),
    CollectionLiteral(CollectionLiteral),
    ThisExpression(ThisExpression),
    SuperExpression(SuperExpression),
    IfExpression(IfExpression),
    WhenExpression(WhenExpression),
    TryExpression(TryExpression),
    JumpExpression(JumpExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParenthesizedExpression {
    pub expression: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralConstant {
    BooleanLiteral(bool),
    IntegerLiteral(String),
    HexLiteral(String),
    BinLiteral(String),
    CharacterLiteral(char),
    RealLiteral(String),
    NullLiteral,
    LongLiteral(String),
    UnsignedLiteral(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StringLiteral {
    LineStringLiteral(LineStringLiteral),
    MultiLineStringLiteral(MultiLineStringLiteral),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LineStringLiteral(pub Vec<LineStringContentOrExpression>);

#[derive(Clone, Debug, PartialEq)]
pub enum LineStringContentOrExpression {
    LineStringContent(LineStringContent),
    LineStringExpression(LineStringExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LineStringContent {
    LineStrText(String),
    LineStrEscapedChar(char),
    LineStrRef(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LineStringExpression {
    pub expression: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MultiLineStringLiteral {
    MultiLineStringContent(MultiLineStringContent),
    MultiLineStringExpression(MultiLineStringExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MultiLineStringContent {
    MultiLineStrText(String),
    MultiLineStringQuote { count: usize },
    MultiLineStrRef(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MultiLineStringExpression {
    pub expression: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallableReference {
    pub receiver_type_op: Option<ReceiverType>,
    pub callable_reference: CallableReference2,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CallableReference2 {
    SimpleIdentifier(String),
    Class,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionLiteral {
    LambdaLiteral(LambdaLiteral),
    AnonymousFunction(AnonymousFunction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnonymousFunction {
    pub this_type_op: Option<Type>,
    pub parameters_with_optional_type: ParametersWithOptionalType,
    pub return_type_op: Option<Type>,
    pub type_constraints_op: Option<TypeConstraints>,
    pub function_body_op: Option<FunctionBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParametersWithOptionalType(pub Vec<FunctionValueParameterWithOptionalType>);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionValueParameterWithOptionalType {
    pub parameter_modifiers_op: Option<ParameterModifiers>,
    pub parameter_with_optional_type: ParameterWithOptionalType,
    pub expression_op: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParameterModifiers(pub Vec<ParameterModifiers2>);

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterModifiers2 {
    Annotation(Annotation),
    ParameterModifier(ParameterModifier),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterModifier {
    VarArg,
    NoInline,
    CrossInline,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParameterWithOptionalType {
    pub simple_identifier: String,
    pub type_op: Option<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeConstraints(pub Vec<TypeConstraint>);

#[derive(Clone, Debug, PartialEq)]
pub struct TypeConstraint {
    pub annotations: Vec<Annotation>,
    pub simple_identifier: String,
    pub type_: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionBody {
    Block(Block),
    Expression(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(pub Statements);

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectLiteral {
    pub delegation_specifiers_op: Option<DelegationSpecifiers>,
    pub class_body_op: Option<ClassBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DelegationSpecifiers(pub Vec<AnnotatedDelegationSpecifier>);

#[derive(Clone, Debug, PartialEq)]
pub struct AnnotatedDelegationSpecifier {
    pub annotations: Vec<Annotation>,
    pub delegation_specifier: DelegationSpecifier,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DelegationSpecifier {
    Constructorinvocation(ConstructorInvocation),
    ExplicitDelegation(ExplicitDelegation),
    UserType(UserType),
    FunctionType(FunctionType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExplicitDelegation {
    pub explicit_delegation: ExplicitDelegation2,
    pub expression: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExplicitDelegation2 {
    UserType(UserType),
    FunctionType(FunctionType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassBody {
    pub class_member_declarations: ClassMemberDeclarations,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassMemberDeclarations(pub Vec<ClassMemberDeclaration>);

#[derive(Clone, Debug, PartialEq)]
pub enum ClassMemberDeclaration {
    Declaration(Declaration),
    CompanionObject(CompanionObject),
    AnonymousInitializer(AnonymousInitializer),
    SecondaryConstructor(SecondaryConstructor),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompanionObject {
    pub modifiers: Option<Modifiers>,
    pub simple_identifier_op: Option<String>,
    pub delegation_specifiers_op: Option<DelegationSpecifiers>,
    pub class_body_op: Option<ClassBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Modifiers(pub Vec<Modifiers2>);

#[derive(Clone, Debug, PartialEq)]
pub enum Modifiers2 {
    Annotation(Annotation),
    Modifier(Modifier),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Modifier {
    ClassModifier(ClassModifier),
    MemberModifier(MemberModifier),
    VisibilityModifier(VisibilityModifier),
    FunctionModifier(FunctionModifier),
    PropertyModifier(PropertyModifier),
    InheritanceModifier(InheritanceModifier),
    ParameterModifier(ParameterModifier),
    PlatformModifier(PlatformModifier),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassModifier {
    Enum,
    Sealed,
    Annotation,
    Data,
    Inner,
    Value,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberModifier {
    Override,
    LateInit,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VisibilityModifier {
    Public,
    Private,
    Internal,
    Protected,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionModifier {
    TailRec,
    Operator,
    Infix,
    Inline,
    External,
    Suspend,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PropertyModifier {
    Const,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InheritanceModifier {
    Abstract,
    Final,
    Open,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PlatformModifier {
    Expect,
    Actual,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnonymousInitializer {
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SecondaryConstructor;

#[derive(Clone, Debug, PartialEq)]
pub struct CollectionLiteral(pub Vec<Expression>);

#[derive(Clone, Debug, PartialEq)]
pub struct ThisExpression;

#[derive(Clone, Debug, PartialEq)]
pub struct SuperExpression;

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpression;

#[derive(Clone, Debug, PartialEq)]
pub struct WhenExpression;

#[derive(Clone, Debug, PartialEq)]
pub struct TryExpression;

#[derive(Clone, Debug, PartialEq)]
pub struct JumpExpression;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub parts: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeProjectionModifier {
    VarianceModifier(VarianceModifier),
    Annotation(Annotation),
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarianceModifier {
    In,
    Out,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AnnotationUseSiteTarget {
    Field,
    Property,
    Get,
    Set,
    Receiver,
    Param,
    SetParam,
    Delegate,
}
